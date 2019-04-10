(*1.subset test*)
let subset_test0 = subset [] [5;7];;
let subset_test1 = subset [4;2;4] [1;2;4];;

(*2.equal set test*)
let equal_sets_test0 = equal_sets [1;3;3;1] [3;1;3];;

(*3.union test*)
let set_union_test0 = equal_sets (set_union [1] [1;2;3]) [1;2;3];;

(*4.intersection test*)
let set_intersection_test0 = equal_sets (set_intersection [1] [1;2;3]) [1];;
let set_intersection_test1 = equal_sets (set_intersection [3;3] [1;2;3]) [3];;

(*5.diff set test*)
let set_diff_test0 = equal_sets (set_diff [1;3;5] [1;4;3;1]) [5];;

(*6.computed_fixed_point test*)
let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 8) 50000 = 0;;


(*7. filter_reachable test*)

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
    B, [T"k"]]

let alpha_grammar = A, alpha_rules

let alpha_test0 =
  filter_reachable alpha_grammar = alpha_grammar;;