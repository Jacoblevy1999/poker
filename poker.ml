type card =
  |Hearts of int
  |Diamonds of int
  |Clubs of int 
  |Spades of int

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
type allseven = card array



let full_deck = Array.of_list [Hearts 0 ; Hearts 1 ; Hearts 2 ; Hearts 3 ; Hearts 4 ; Hearts 5 ; Hearts 6
                              ; Hearts 7 ; Hearts 8 ; Hearts 9 ; Hearts 10 ; Hearts 11 ; Hearts 12 ;
                               Diamonds 0 ; Diamonds 1 ; Diamonds 2 ; Diamonds 3 ; Diamonds 4 ; Diamonds 5 ; Diamonds 6
                              ; Diamonds 7 ; Diamonds 8 ; Diamonds 9 ; Diamonds 10 ; Diamonds 11 ; Diamonds 12 ;
                               Spades 0 ; Spades 1 ; Spades 2 ; Spades 3 ; Spades 4 ; Spades 5 ; Spades 6
                              ; Spades 7 ; Spades 8 ; Spades 9 ; Spades 10 ; Spades 11 ; Spades 12 ;
                               Clubs 0 ; Clubs 1 ; Clubs 2 ; Clubs 3 ; Clubs 4 ; Clubs 5 ; Clubs 6
                              ; Clubs 7 ; Clubs 8 ; Clubs 9 ; Clubs 10 ; Clubs 11 ; Clubs 12 ;]

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

let rec insert_freq_list v acc = 
  match acc with 
  | [] -> ((v, 1)::acc)
  |(v1,c)::t -> if v = v1 then (v, c+1)::t else 
      (v1, c)::(insert_freq_list v t)

let rec suits_in_hand (c:allseven) (acc:(string * int) list)= 
  match Array.to_list c with 
  |[] -> acc
  |h::t -> begin 
      match h with
      | Hearts _ -> suits_in_hand (Array.of_list t) (insert_freq_list "h" acc)
      | Spades _ -> suits_in_hand (Array.of_list t) (insert_freq_list "s" acc)
      | Diamonds _ -> suits_in_hand (Array.of_list t) (insert_freq_list "d" acc)
      | Clubs _ -> suits_in_hand (Array.of_list t) (insert_freq_list "c" acc)
    end

let rec values_in_hand (c:allseven) acc= 
  match Array.to_list c with 
  |[] -> acc
  |h::t -> begin 
      match h with
      | Hearts i -> values_in_hand (Array.of_list t) (insert_freq_list i acc)
      | Spades i -> values_in_hand (Array.of_list t) (insert_freq_list i acc)
      | Diamonds i -> values_in_hand (Array.of_list t) (insert_freq_list i acc)
      | Clubs i -> values_in_hand (Array.of_list t) (insert_freq_list i acc)
    end

let rec is_flush (c:allseven) = 
  match suits_in_hand c [] with
  |[] -> false
  |(k,v)::(k1, v1)::(k2, v2)::t -> if v >= 5 || v1 >= 5 || v2 >= 5 then true else false
  |(k,v)::(k1, v1)::t -> if v >= 5 || v1 >= 5 then true else false
  |(k,v)::t -> if v >= 5 then true else false

