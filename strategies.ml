open Poker
open Command
open State

(** [easy_strat st] is the following move of the computer, given the currrent state.
    For this particularly easy strategy, that means calling all bets, and checking otherwise.*)
let easy_strat (st:State.t) : Command.move = 
  if st.previous_move = [] then Check else 
    match List.hd st.previous_move with
    |Bet _ -> Call
    |Raise _ -> Call
    |_ -> Check

(**  [floor_five n] is 5 if [n] is less than 5, and n otherwise*)
let floor_five n = 
  if n < 5 then 5 else n

(** [ceil f] is the ceiling function*)
let ceil (f:float) = 
  if mod_float f 1.= 0. then int_of_float f else (int_of_float (f +. 1.))

(** [chen_strength in_hand] is the Chen strength of the preflop hand of a player,
    according to the algorithm designed by Bill Chen.*)
let chen_strength (in_hand:card list) : int= 
  let highest = List.hd (values_in_hand (highest_n 1 in_hand [])) in
  let highest_val = if highest = 12 then float_of_int 10 else if highest = 11 then float_of_int 8
    else if highest = 10 then float_of_int 7 else if highest = 9 then float_of_int 6 else
      (float_of_int highest)/.2. in 
  let suited = if List.hd (suits_in_hand in_hand) = List.hd (List.tl (suits_in_hand in_hand))
    then true else false in
  let pair = if (pairs in_hand []) <> [] then true else false in
  let snd = List.hd (List.tl (values_in_hand in_hand)) in 
  let gap = (highest - snd)-1 in
  let gap_pts = if gap = 0 then (-1) else if gap = 1 || gap = -1 then 0 else if gap = 2 then 
      2 else if gap = 3 then 4 else 5 in 
  if pair then (floor_five (int_of_float (2. *. highest_val))) - (gap_pts) else
  if suited then (ceil highest_val) + 2 - (gap_pts) else
    (ceil highest_val) - (gap_pts)

(** [remove_from_deck card deck] removes [card] from the [deck]*)
let rec remove_from_deck cards deck = 
  match deck with
  |h::t -> if List.mem h cards then remove_from_deck cards t else h::(remove_from_deck cards t)
  |[] -> []

let rec simulate_hand (table:Poker.table) (hand:Poker.hand) : int = 
  let used = Array.to_list (Array.concat [table;hand]) in
  let remaining = remove_from_deck used (Array.to_list Poker.full_deck) in
  let shuffled = Poker.shuffle (Array.of_list remaining) (Array.of_list []) in
  let p2_hand = snd (Poker.deal shuffled (Array.of_list [])) in
  let used = Array.to_list (Array.concat [p2_hand;(Array.of_list used)]) in
  let remaining = remove_from_deck used (Array.to_list Poker.full_deck) in
  if Array.length table = 5 then if Poker.winner hand p2_hand table = "player 1" then 1 else 0
  else if Array.length table = 4 then let table = Poker.river (Array.of_list remaining) table in
    if Poker.winner hand p2_hand table = "player 1" then 1 else 0 
  else if Array.length table = 3 then let table1 = Poker.river (Array.of_list remaining) table in
    simulate_hand table1 hand else failwith "invalid simulation"

let rec pct_chance wins tries remaining table hand = 
  if remaining = 0 then (float_of_int wins)/.(float_of_int tries) else
  if simulate_hand table hand = 1 then pct_chance (wins+1) (tries+1)
      (remaining-1) table hand else pct_chance (wins) (tries+1)
      (remaining-1) table hand

(** [bet_range min max] takes in two ints, [min] and [max],
    and returns an int between them*)
let bet_range min max = 
  Random.self_init();
  min + (Random.int max-min)

(** [preflop_bet last pot in_hand] is the appropriate preflop action for the
    CPU to take given the most recent move [last], the current size of the
    [pot] and what cards the player has [in_hand]. *)
let preflop_bet last pot in_hand : Command.move=
  Random.self_init();
  let strength = chen_strength in_hand in
  match last with
  |Some Check
  |None -> if strength >= 12 then Bet (bet_range (pot) (pot*3)) else if strength >= 10
    then Bet (bet_range (pot/2) (pot*2)) else let r = Random.float 1. in if r < 0.8 then
        Check else Bet (bet_range (pot/2) (pot*2))
  | _ -> let r = Random.float 1. in if strength >= 12 then if r > 0.9 then Call else
        Raise (bet_range (pot) (pot*2)) else if strength >= 10
    then if r > 0.95 then Raise (bet_range (pot) (pot*2)) else if 
        r > 0.85 then Fold else Call else if r < 0.8 then
      Fold else Raise (bet_range (pot/2) (pot*2))

(** [med_strat st] is the following move of the computer, given the currrent state,
    according to an informed betting strategy. *)
let med_strat (st:State.t) : Command.move = 
  let last = if st.previous_move = [] then None else
      Some (List.hd st.previous_move) in
  let num_cards = Array.length st.table + Array.length st.hand1 in
  if num_cards = 2 then preflop_bet last st.pot (Array.to_list st.hand1) else
    match last with
    |Some Check
    |None -> let chance = (pct_chance 0 0 1000 st.table st.hand1) in
      Random.self_init(); let r = Random.float 1. in
      if chance > 0.85 then Bet (bet_range (st.pot/3) st.pot*4) else 
      if chance > 0.75 then Bet (bet_range (st.pot/3) st.pot*2) else 
      if chance > 0.65 then Bet (bet_range (st.pot/4) st.pot) else 
      if chance > 0.50 then Bet (bet_range (st.pot/8) 2*st.pot/3) else 
      if chance < 0.25 then Fold else
      if r > 0.9 then Bet (bet_range (st.pot/3) st.pot*3) else Fold
    |_ -> let chance = (pct_chance 0 0 1000 st.table st.hand1) in
      Random.self_init(); let r = Random.float 1. in
      if chance > 0.85 then if r > 0.85 then Call else Raise (bet_range (st.pot/3) st.pot*3) else 
      if chance > 0.75 then if r > 0.4 then Call else Raise (bet_range (st.pot/4) st.pot*2) else 
      if chance > 0.65 then if r > 0.25 then Call else Bet (bet_range (st.pot/4) st.pot) else 
      if chance > 0.50 then if r > 0.4 then Call else 
        if r > 0.85 then Raise (bet_range (st.pot/8) st.pot) else Fold else
      if chance < 0.25 then Fold else
      if r > 0.9 then Raise (bet_range (st.pot/3) st.pot*3) else Fold


(* 
let ucb (tot_reward:int) (ind_pulls:int) (total_pulls:int) : float =
  let r = float_of_int tot_reward in
  let n = float_of_int ind_pulls in
  let t  = float_of_int total_pulls in
  (r/.n) +. Float.sqrt (2. *. (Float.log n) /. t) *)


(* (** A [gamestate] is a state representing your hand quality,
 *)
   type gamestate = 


   (** A [gametree] is either Leaf, or a Node with a string representing whose turn
      it is, an int representing *)
   type gametree = 
   |Leaf of int
   |Node of string * int * int * gametree * gametree * gametree * gametree


   let create_tree *)
