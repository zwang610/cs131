(* 1. *)
let rec subset a b = 
    match a with
    | [] -> true
    | h::t -> if List.mem h b then subset t b 
              else false;;

(* 2. *)
let equal_sets a b = subset a b && subset b a;;

(* 3. *)
let rec set_union a b =
    match a with
    | [] -> b
    | h::t -> if List.mem h b then set_union t b
              else h::set_union t b;;

(* 4. *)
let rec set_intersection a b =
    List.filter (fun e -> List.mem e a) b;;

(* 5. *)
let rec set_diff a b =
    List.filter (fun e -> not (List.mem e b)) a;;

(* 6. *)
let rec computed_fixed_point eq f x =
    if (eq (f x) x) then x
    else computed_fixed_point eq f (f x);;

(* 7 *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let rec reachable_rules ss all_rules reached_rules rules=
        (*find the first rule with left-hand side = ss,
        use search_rhs function to find the old and new reachable rules by the right-hand side,
        recursive call with rules parameter = the rest rules, reached_rules = old and new reachable_rules *)
        match rules with
        | rh::rt -> if ((ss = (fst rh)) && not(List.mem rh reached_rules)) 
                        then (reachable_rules ss all_rules (search_rhs (snd rh) all_rules (rh::reached_rules)) rt)
                    else reachable_rules ss all_rules reached_rules (set_diff rt reached_rules)
        | [] -> reached_rules

    (*return a list of new reachable rules in the right-hand side list + reached_rules*)
    and search_rhs rhs all_rules reached_rules = 
        (*find the first nonterminals in right-hand side,
        use reachable_rules function to find the reachable with this nonterminals,
        combine old reached_rules with the return new reached_rules,
        recursive call with rhs = the rest rhs, reached_rules = old and new reached_rules*)
        match rhs with
        | (T _)::rhst -> search_rhs rhst all_rules reached_rules
        | (N ss)::rhst -> set_union (search_rhs rhst all_rules reached_rules) (reachable_rules ss all_rules reached_rules all_rules)
        | [] -> reached_rules

(*return a list of new reachable rules by ss + reached_rules*)
let filter_reachable grammar =
    let start_symbol = fst grammar and rules_list = snd grammar in
    (*set_intersection here is only used for output the rules in original order*)
    (start_symbol, (set_intersection (reachable_rules start_symbol rules_list [] rules_list) rules_list));;