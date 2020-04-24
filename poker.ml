type card = (int * string)

type besthand = 
  |RoyalFlush
  |StraightFlush of int
  |FourOfKind of int
  |FullHouse of int * int
  |Flush of int *  int * int * int * int
  |Straight of int
  |ThreeOfKind of int
  |TwoPair of int * int * int
  |Pair of int * int * int * int
  |HighCard of int *  int * int * int * int


type deck = card array
type hand = card array
type table = card array
type allseven = card list



let (full_deck:deck) = Array.of_list [(0, "h");(1, "h");(2, "h");(3, "h");(4, "h");
                                      (5, "h");(6, "h");(7, "h");(8, "h");(9, "h");
                                      (10, "h");(11, "h");(12, "h");(0, "d");(1, "d");
                                      (2, "d");(3, "d");(4, "d");(5, "d");(6, "d");
                                      (7, "d");(8, "d");(9, "d");(10, "d");(11, "d");
                                      (12, "d");(0, "c");(1, "c");(2, "c");(3, "c");(4, "c");
                                      (5, "c");(6, "c");(7, "c");(8, "c");(9, "c");
                                      (10, "c");(11, "c");(12, "c");(0, "s");(1, "s");(2, "s");
                                      (3, "s");(4, "s");(5, "s");(6, "s");(7, "s");(8, "s");(9, "s");
                                      (10, "s");(11, "s");(12, "s")]

let rec shuffle (fulldeck: card array) (acc:deck) : deck =
  Random.self_init(); 
  if Array.length acc = 9 then acc else
    let pos = Random.int (Array.length fulldeck) in 
    shuffle (Array.concat [(Array.sub fulldeck 0 pos);(Array.sub fulldeck (pos+1) ((Array.length fulldeck)-pos-1))]) 
      (Array.of_list (fulldeck.(pos)::(Array.to_list acc)))

let rec deal (deck:deck) (hand:hand) : deck * hand = 
  if Array.length hand = 2 then (deck, hand) else
    deal (Array.sub deck 1 (Array.length deck-1)) 
      (Array.of_list (deck.(0)::(Array.to_list hand)))

let rec flop (deck:deck) (table:table) : deck * table = 
  if Array.length table = 3 then (deck, table) else
    flop (Array.sub deck 1 (Array.length deck-1)) 
      (Array.concat [(table);(Array.sub deck 0 1)])

let turn (deck:deck) (table:table) : deck * table = 
  ((Array.sub deck 1 1),
   (Array.concat [(table);(Array.sub deck 0 1)]))

let river (deck:deck) (table:table) : table = 
  (Array.concat [(table);(Array.sub deck 0 1)])

let rec freq elt lst acc= 
  match lst with 
  | [] -> acc
  |h::t -> if h = elt then freq elt t (acc+1) else freq elt t acc

let helper_comp (k1, v1) (k2, v2) = 
  (-1)*(compare k1 k2)

let suits_in_hand c= 
  snd (List.split c)

let rec values_in_hand c= 
  fst (List.split c)

let rec minimum lst acc= 
  match lst with 
  |[] -> acc
  |h::t -> if acc=0 then minimum t h else if h<acc then minimum t h
    else minimum t acc

let rec replace lst old n=
  match lst with 
  |(k,v)::t -> if k = old then n::t else (k,v)::(replace t old n)
  |_ -> failwith "not in list"

let rec highest_n n c acc= 
  match c with
  | [] -> acc
  | (k,v)::t -> if List.length acc < n then highest_n n t ((k,v)::acc) else
      let m = minimum (values_in_hand acc) 0 in 
      if k > m then highest_n n t (replace acc m (k,v)) else highest_n n t acc

let rec add_all_value v (c) acc=
  match c with
  |[] -> acc
  |(k1,v1)::t -> if k1 = v then add_all_value v t ((k1,v1)::acc) else
      add_all_value v t acc

let rec add_all_not_value v c acc=
  match c with
  |[] -> acc
  |(k1,v1)::t -> if k1 <> v then add_all_not_value v t ((k1,v1)::acc) else
      add_all_not_value v t acc

let rec add_all_suit s (c) acc=
  match c with
  |[] -> acc
  |(k,v)::t -> if v = s then add_all_suit s t ((k,v)::acc) else
      add_all_suit s t acc

let rec all_flush (c) (acc)= 
  match c with
  |[] -> acc
  |(k,v)::t -> if freq v (suits_in_hand c) 0 < 5 then all_flush t acc
    else add_all_suit v c []

let best_flush c = 
  if all_flush c [] = [] then [] else
    highest_n 5 (all_flush c []) []

let rec best_four_kind (c) (acc)=
  match c with
  |[] -> acc
  |(k,v)::t -> if freq k (values_in_hand c) 0 < 4 then best_four_kind t acc
    else (add_all_value k c [])@(highest_n 1 (add_all_not_value k c []) [])

let best_straight (c) (acc)=
  let rec straight_sorter lst acc = 
    match lst with
    |[] -> if List.length acc >= 5 then (Array.to_list (Array.sub (Array.of_list (List.rev acc)) 0 5))
      else if List.length acc = 4 && fst (List.hd acc) = 0 && List.mem 12 (values_in_hand c)then 
        (List.hd (add_all_value 12 c []))::(Array.to_list (Array.sub (Array.of_list (List.rev acc)) 0 4))
      else []
    |(k,v)::t -> if acc = [] then straight_sorter t [(k,v)] else
      if k = 1+(fst (List.hd acc)) then straight_sorter t ((k,v)::acc) else straight_sorter t [(k,v)]
  in straight_sorter (List.rev (List.sort_uniq helper_comp c)) []

let best_straight_flush c = 
  if all_flush c [] <> [] && best_straight (all_flush c []) [] <> [] then 
    best_straight (all_flush c []) [] else []

let is_royal_flush c = 
  if best_straight_flush c <> [] && fst (List.hd (best_straight_flush c)) = 12 
  then best_straight_flush c else []

let rec three_kind c acc = 
  match c with
  |[] -> []
  |(k,v)::t -> if freq k (values_in_hand c) 0 < 3 then three_kind t acc
    else let l = List.merge helper_comp (List.sort helper_comp (add_all_value k c [])) 
             (List.sort helper_comp (three_kind t []))
      in (List.nth l 0)::(List.nth l 1)::(List.nth l 2)::[]

let best_three_kind c = 
  if three_kind c [] = [] then [] else
    let fst_3 = three_kind c [] in 
    fst_3@(highest_n 2 (add_all_not_value (fst (List.hd fst_3)) c []) [])

let rec pairs c acc = 
  match c with
  |[] -> []
  |(k,v)::t -> if (freq k (values_in_hand c) 0) < 2 then pairs t acc
    else let l = List.merge helper_comp (List.sort helper_comp (add_all_value k c [])) 
             (List.sort helper_comp (pairs t []))
      in if List.length l > 4 then 
        (List.nth l 0)::(List.nth l 1)::(List.nth l 2)::(List.nth l 3)::[] else
        l

let two_pair c = 
  let p = pairs c [] in
  if List.length p = 4 then p@(highest_n 1 (add_all_not_value (fst (List.hd p)) c []) [])
  else []

let one_pair c = 
  let p = pairs c [] in
  if List.length p = 2 then 
    p@(highest_n 3 (add_all_not_value (fst (List.hd p)) c []) [])
  else []

let best_fh c = 
  let tk = (three_kind c []) in
  if tk <> [] && pairs (add_all_not_value (fst (List.hd tk)) c []) [] <> [] then
    let p = Array.to_list (Array.sub (Array.of_list
                                        (pairs (add_all_not_value 
                                                  (fst (List.hd tk)) c []) [])) 0 2) in 
    tk@p 
  else []

let rec first_non_empty (a:('a list) array) (index:int) = 
  if a.(index) <> [] then index else first_non_empty a (index+1)

let rec max lst acc= 
  match lst with
  |[] -> acc
  |h::t -> if h>acc then max t h else max t acc

let rec break_high h1 h2 = 
  match h1, h2 with
  |[], [] -> "tie"
  |_, _ -> let m1  = max (values_in_hand h1) 0 in
    let m2 = max (values_in_hand h2) 0 in
    if m1 > m2 then "player 1" else 
    if m1 < m2 then "player 2" else
      break_high (List.remove_assoc (m1) h1) (List.remove_assoc (m2) h2)

let break_group n h1 h2 = 
  if break_high (Array.to_list (Array.sub (Array.of_list h1) 0 n)) 
      (Array.to_list (Array.sub (Array.of_list h2) 0 n)) <> "tie" then
    break_high (Array.to_list (Array.sub (Array.of_list h1) 0 n)) 
      (Array.to_list (Array.sub (Array.of_list h2) 0 n)) else 
    break_high h1 h2

let break_mult_group n1 n2 h1 h2 = 
  if break_high (Array.to_list (Array.sub (Array.of_list h1) 0 n1)) 
      (Array.to_list (Array.sub (Array.of_list h2) 0 n1)) <> "tie" then
    break_high (Array.to_list (Array.sub (Array.of_list h1) 0 n1)) 
      (Array.to_list (Array.sub (Array.of_list h2) 0 n1)) else
  if break_high (Array.to_list (Array.sub (Array.of_list h1) n1 n2)) 
      (Array.to_list (Array.sub (Array.of_list h2) n1 n2)) <> "tie" then
    break_high (Array.to_list (Array.sub (Array.of_list h1) n1 n2)) 
      (Array.to_list (Array.sub (Array.of_list h2) n1 n2)) else
    break_high h1 h2


let winner (h1) (h2) (t): string= 
  let hand1 = Array.to_list (Array.concat [h1;t]) in
  let hand2 = Array.to_list (Array.concat [h2;t]) in
  let hchy1 = Array.of_list [is_royal_flush hand1;best_straight_flush hand1
                            ;best_four_kind hand1 [];best_fh hand1;best_flush hand1;
                             best_straight hand1 [];best_three_kind hand1;two_pair hand1;
                             one_pair hand1;highest_n 5 hand1 []] in
  let hchy2 = Array.of_list [is_royal_flush hand2;best_straight_flush hand2
                            ;best_four_kind hand2 [];best_fh hand2;best_flush hand2;
                             best_straight hand2 [];best_three_kind hand2;two_pair hand2;
                             one_pair hand2;highest_n 5 hand2 []] in
  if (first_non_empty hchy1 0) < (first_non_empty hchy2 0) then "player 1" else
  if (first_non_empty hchy1 0) > (first_non_empty hchy2 0) then "player 2" else
    let r = first_non_empty hchy1 0 in if r = 0 then "tie" else if r = 1 then
      break_high (best_straight_flush hand1) (best_straight_flush hand2) else if r = 2 then
      break_group 4 (best_four_kind hand1 []) (best_four_kind hand2 []) else if r = 3 then
      break_mult_group 3 2 (best_fh hand1) (best_fh hand2) else if r = 4 then
      break_high (best_flush hand1) (best_flush hand2) else if r=5 then
      break_high (best_straight hand1 []) (best_straight hand2 []) else if r=6 then
      break_group 3 (best_three_kind hand1) (best_three_kind hand2) else if r=7 then
      break_mult_group 2 2 (two_pair hand1) (two_pair hand2) else if r=8 then
      break_group 2 (one_pair hand1) (one_pair hand2) else 
      break_high (highest_n 5 hand1 []) (highest_n 5 hand2 [])


let d = shuffle full_deck (Array.of_list [])
let h1 = deal d (Array.of_list [])
let h2 = deal (fst h1) (Array.of_list [])
let f = flop (fst h2) (Array.of_list [])
let t = turn (fst f) (snd f)
let r = river (fst t) (snd t)
let fs1 = snd h1
let fs2 = snd h2


let val_dict  = Array.of_list ["two of";"three of";"four of";"five of";"six of";"seven of";
                               "eight of";"nine of";"ten of";"jack of";"queen of";"king of";"ace of"]

let rec format_lst c= 
  let rec w_commas = function
    |[] -> ""
    |(k,v)::t -> if v = "h" then (val_dict.(k))^" hearts, "^(w_commas t) else
      if v = "d" then (val_dict.(k))^" diamonds, "^(w_commas t) else
      if v = "s" then (val_dict.(k))^" spades, "^(w_commas t) else
        (val_dict.(k))^" clubs, "^(w_commas t) in
  String.sub (w_commas c) 0 (String.length (w_commas c)-2)

