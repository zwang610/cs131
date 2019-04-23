type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(*1.*)
let rec prod_func rules result symb =
    match rules with
    | [] -> result
    | (expr, rhs)::rt -> if expr = symb then prod_func rt (rhs::result) symb
                                      else prod_func rt result symb

let convert_grammar old_grammar = 
    match old_grammar with
    | (start_symbol, rules) -> (start_symbol, prod_func rules [])

(*2.*)
let rec find_leaves ptree_list =
    match ptree_list with
    | [] -> []
    | (Node (_, p))::t -> (find_leaves p)@(find_leaves t)
    | (Leaf l)::t -> l::(find_leaves t);;

let parse_tree_leaves ptree = 
    match ptree with
    | (Node (_, ptree_list)) -> find_leaves ptree_list
    | (Leaf l) -> [l];;

(*3.*)
let rec slist_match prodf slist = 
    match slist with
    | [] -> (fun accept frag -> accept frag) 
    | (T s)::st -> (fun accept frag ->
                    match frag with
                    | [] -> None
                    | fh::ft -> if fh = s then slist_match prodf st accept ft 
                                else None )
    | (N ns)::st -> (fun accept frag ->
                    for_all_replace prodf (prodf ns) st accept frag)

    and for_all_replace prodf rhs tail=
    match rhs with
    | [] -> (fun accept frag -> None)
    | rs::rt -> (fun accept frag -> 
                let x = slist_match prodf (rs@tail) accept frag in
                match x with
                | None -> for_all_replace prodf rt tail accept frag 
                | _ -> x ) 

let make_matcher gram = 
    let start_symb = fst gram and prodf = snd gram in
    for_all_replace prodf (prodf start_symb) []

(*4.*)
let rec get_replace prodf rhs tail frag= 
        match rhs with
        | [] -> []
        | rs::rt -> let acceptor string = Some string in
                    match slist_match prodf (rs@tail) acceptor frag with
                    | None -> get_replace prodf rt tail frag
                    | _ -> rs

    and unused_frag used_frag frag =
        match used_frag with
        | [] -> frag
        | uh::ut -> (match frag with
                    | [] -> []
                    | fh::ft -> unused_frag ut ft)

    and get_tree prodf start_symb tail frag=
        match start_symb with
        | (T s) -> (Leaf s)
        | (N ns) ->let replace = get_replace prodf (prodf ns) tail frag in
                  (Node (ns, (get_branch prodf replace tail frag)))
    and get_branch prodf slist tail frag=
        match slist with
        | [] -> []
        | sh::st -> let tree = get_tree prodf sh (st@tail) frag in
                    let new_frag = unused_frag (parse_tree_leaves tree) frag in
                    tree::(get_branch prodf st tail new_frag)

let make_parser gram frag = 
    let start_symb = fst gram and prodf = snd gram in
    let acceptor string = Some string in
    match make_matcher gram acceptor frag with
    | None -> None
    | _ -> Some(get_tree prodf (N start_symb) [] frag)