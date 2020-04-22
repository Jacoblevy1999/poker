open Poker
open OUnit2

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
                                                               First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
                                                                                                                           though not necessarily in the same order. We credit the author of A2.test
    for writing this function. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2


(* let make_win_test 
    (name :string)
    (h1 : hand) 
    (h2: hand) 
    (t: table)
    (expected_output : string) : test = 
   name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (winner h1 h2 t)) *)
let hand i1 i2 = 
  (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1)])

let table i1 i2 i3 i4 i5 i6 i7= 
  (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1);
                 (Array.sub full_deck i3 1);(Array.sub full_deck i4 1);(Array.sub full_deck i5 1);
                 (Array.sub full_deck i6 1);(Array.sub full_deck i7 1)])

let b_five i1 i2 i3 i4 i5 = 
  (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1);
                 (Array.sub full_deck i3 1);(Array.sub full_deck i4 1);(Array.sub full_deck i5 1)])

let hand_lst i1 i2 = 
  Array.to_list (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1)])

let table_lst i1 i2 i3 i4 i5 i6 i7= 
  Array.to_list (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1);
                               (Array.sub full_deck i3 1);(Array.sub full_deck i4 1);(Array.sub full_deck i5 1);
                               (Array.sub full_deck i6 1);(Array.sub full_deck i7 1)])

let b_five_lst i1 i2 i3 i4 i5 = 
  Array.to_list (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1);
                               (Array.sub full_deck i3 1);(Array.sub full_deck i4 1);(Array.sub full_deck i5 1)])

let tests = "poker test suite" >::: [
    "flush"  >:: (fun _ -> assert_equal true (cmp_set_like_lists 
                                                (b_five_lst 11 12 9 10 8) (highest_n 5 (table_lst 1 2 8 9 10 11 12) [])));
    "higher flush" >:: (fun _ -> assert_equal "player 1" (winner (hand 12 11) (hand 10 9) (b_five 0 1 2 3 4)));
    "high card same 2 pair" >:: (fun _ -> assert_equal "player 1" (winner (hand 48 51) (hand 50 49) (b_five 0 1 3 14 13)));
    "flush beats straight" >:: (fun _ -> assert_equal "player 1" (winner (hand 0 10) (hand 50 49) (b_five 0 1 3 14 13)));
    "straight beats pair" >:: (fun _ -> assert_equal "player 2" (winner (hand 0 51) (hand 20 21) (b_five 4 5 6 14 13)));
  ]




let _ = run_test_tt_main tests