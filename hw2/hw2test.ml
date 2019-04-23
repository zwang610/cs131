(*1. test on function convert grammar*)
let rec subset a b = 
    match a with
    | [] -> true
    | h::t -> if List.mem h b then subset t b 
              else false;;

let equal_sets a b = subset a b && subset b a;;

type uppercase =
  | A | B | C | D | E

let alpha_rules =
   [A, [T"a"; N A; T"b"];
    A, [N B];
    A, [N A; N C; N A];
    A, [N D];
    A, [N E; N D];
    A, [N D; N E];
    E, [T"b"];
    E, [T"v"];
    B, [T"c"];
    B, [T"k"];
    D, [T"d"];
    E, [T"e"]
    ]

let alpha_grammar = A, alpha_rules

let nalpha_grammar = convert_grammar alpha_grammar;;
let p_func = (snd(nalpha_grammar));;

let prob1_test0 = (equal_sets (p_func A) [[T"a"; N A; T"b"];
                                        [N B];[N A; N C; N A];
                                        [N D];[N E; N D];
                                        [N D; N E]]);;

(*2. test on function parse_tree_leaves*)
let prob2_test0 = (equal_sets ["a";"b";"c"] (parse_tree_leaves (Node (A, 
                                                                    [Node(B, [Leaf "a"; Leaf "b"]);
                                                                    Leaf "c"]))))

(*3. test on make_matcher*)
let nalpha_grammar =
  (A,
   function
     | A -> [[N B; N C; N A]; [N B]]
     | B -> [[N D;N E;T"b"]]
     | E -> [[T"$"; T"e"]]
     | C -> [[T"+"];[T"-"]]
     | D -> [[T"0"]])
let accept_all string = Some string
let prob3_test0 = ((make_matcher nalpha_grammar accept_all ["9"]) = None)
let prob3_test1 = ((make_matcher nalpha_grammar accept_all ["0";"$";"e";"b"]) = Some [])
let prob3_test2 = (make_matcher nalpha_grammar accept_all ["0";"$";"e";"b";"+"] = Some ["+"])
(*4. test on make_parser*)
let prob4_test0 = 
(make_parser nalpha_grammar ["0";"$";"e";"b"] = Some
 (Node (A,
   [Node (B,
     [Node (D, [Leaf "0"]); Node (E, [Leaf "$"; Leaf "e"]); Leaf "b"])])))
let prob4_test1 = (make_parser nalpha_grammar ["0";"$";"e";"b";"+"] = None)