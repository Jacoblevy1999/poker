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



let full_deck = Array.of_list [(0, "h");(1, "h");(2, "h");(3, "h");(4, "h");
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
      (Array.of_list (deck.(0)::(Array.to_list table)))

let turn (deck:deck) (table:table) : deck * table = 
  ((Array.sub deck 1 1),
   (Array.of_list (deck.(0)::(Array.to_list table))))

let river (deck:deck) (table:table) : table = 
  (Array.of_list (deck.(0)::(Array.to_list table)))

let rec freq elt lst acc= 
  match lst with 
  | [] -> acc
  |h::t -> if h = elt then freq elt t (acc+1) else freq elt t acc

let suits_in_hand (c:allseven)= 
  snd (List.split c)

let rec values_in_hand (c:allseven)= 
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
      if k < m then highest_n n t (replace acc m (k,v)) else highest_n n t acc

let rec add_all_value v (c:allseven) acc=
  match c with
  |[] -> acc
  |(k1,v1)::t -> if k1 = v then add_all_value v t ((k1,v1)::acc) else
      add_all_value v t acc

let rec add_all_not_value v (c:allseven) acc=
  match c with
  |[] -> acc
  |(k1,v1)::t -> if k1 <> v then add_all_not_value v t ((k1,v1)::acc) else
      add_all_not_value v t acc

let rec add_all_suit s (c:allseven) acc=
  match c with
  |[] -> acc
  |(k,v)::t -> if v = s then add_all_suit s t ((k,v)::acc) else
      add_all_suit s t acc

let rec best_flush (c:allseven) (acc)= 
  match c with
  |[] -> acc
  |(k,v)::t -> if freq v (suits_in_hand c) 0 < 5 then best_flush t acc
    else if freq v (suits_in_hand c) 0 = 5 then add_all_suit v c [] else
      highest_n 5 (add_all_suit v c []) []

let rec best_four_kind (c:allseven) (acc)=
  match c with
  |[] -> acc
  |(k,v)::t -> if freq k (values_in_hand c) 0 < 4 then best_four_kind t acc
    else (add_all_value k c [])@(highest_n 1 (add_all_not_value k c []) [])

let helper_comp (k1, v1) (k2, v2) = 
  compare k1 k2

let best_straight (c:allseven) (acc)=
  let rec straight_sorter lst acc = 
    match lst with
    |[] -> if List.length acc >= 5 then (Array.to_list (Array.sub (Array.of_list acc) 0 5))
      else []
    |(k,v)::t -> if acc = [] then straight_sorter t [(k,v)] else
      if k = 1+(fst (List.hd acc)) then straight_sorter t ((k,v)::acc) else straight_sorter t [(k,v)]
  in straight_sorter (List.sort_uniq helper_comp c) []

let best_straight_flush c = 
  if best_flush c [] <> [] && best_straight c [] <> [] then best_straight c [] else []

let is_royal_flush c = 
  if best_straight_flush c <> [] && fst (List.hd (best_straight_flush c)) = 12 
  then best_straight_flush c else []

let helper_comp (e1) (e2) = 
  match e1, e2 with
  |(k1, v1), (k2, v2) -> if k1 < k2 then 1 else if k1 > k2 then -1 else 0

let rec three_kind c acc = 
  match c with
  |[] -> []
  |(k,v)::t -> if freq k (values_in_hand c) 0 < 3 then three_kind t acc
    else let l = List.merge helper_comp (add_all_value k c []) (three_kind t [])
      in (List.nth l 0)::(List.nth l 1)::(List.nth l 2)::[]

let rec pairs c acc = 
  match c with
  |[] -> []
  |(k,v)::t -> if (freq k (values_in_hand c) 0) < 2 then pairs t acc
    else let l = List.merge helper_comp (add_all_value k c []) (pairs t [])
      in if List.length l > 4 then 
        (List.nth l 0)::(List.nth l 1)::(List.nth l 2)::(List.nth l 3)::[] else
        l

let two_pair c = 
  let p = pairs c [] in
  if List.length p >= 4 then p else []

let one_pair c = 
  let p = pairs c [] in
  if List.length p = 2 then (List.nth p 0)::(List.nth p 1)::[] else
    []

let best_fh c = 
  let tk = (three_kind c []) in
  if tk <> [] && one_pair (add_all_not_value (fst (List.hd tk)) c []) <> [] then
    let p = one_pair (add_all_not_value (fst (List.hd tk)) c []) in 
    tk@p 
  else []

let rec first_non_empty (a:('a list) array) (index:int) = 
  if a.(index) <> [] then index else first_non_empty a (index+1)

let winner (h1:hand) (h2:hand) (t:table): string= 
  let hand1 = Array.to_list (Array.concat [h1;t]) in
  let hand2 = Array.to_list (Array.concat [h2;t]) in
  let hchy1 = Array.of_list [is_royal_flush hand1;best_straight_flush hand1
                            ;best_four_kind hand1 [];best_fh hand1;best_flush hand1 [];
                             best_straight hand1 [];three_kind hand1 [];two_pair hand1;
                             one_pair hand1;highest_n 5 hand1 []] in
  let hchy2 = Array.of_list [is_royal_flush hand2;best_straight_flush hand2
                            ;best_four_kind hand2 [];best_fh hand2;best_flush hand2 [];
                             best_straight hand2 [];three_kind hand2 [];two_pair hand2;
                             one_pair hand2;highest_n 5 hand2 []] in
  if (first_non_empty hchy1 0) < (first_non_empty hchy2 0) then "player 1" else
  if (first_non_empty hchy1 0) > (first_non_empty hchy2 0) then "player 2" else "tie"

