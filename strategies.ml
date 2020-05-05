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
  let gap = (highest - snd)+1 in
  let gap_pts = if gap = 0 then (-1) else if gap = 1 then 1 else if gap = 2 then 
      2 else if gap = 3 then 4 else 5 in 
  if pair && suited then (floor_five (int_of_float (2. *. highest_val))) + 2 else
  if pair then (floor_five (int_of_float (2. *. highest_val))) - (gap_pts) else
  if suited then (ceil highest_val) + 2 - (gap_pts) else
    (ceil highest_val) - (gap_pts)


(** [med_strat st] is the following move of the computer, given the currrent state,
    according to an informed betting strategy. *)
(* let med_strat (st:State.t) : Command.move = 
*)


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
