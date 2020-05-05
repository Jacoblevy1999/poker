open Poker
open Command
open State

(** [easy_strat prev] is the following move of the computer, given the previous move.
    For this particularly easy strategy, that means calling all bets, and checking otherwise.*)
let easy_strat (st:State.t) : Command.move = 
  if st.previous_move = [] then Check else 
    match List.hd st.previous_move with
    |Bet _ -> Call
    |Raise _ -> Call
    |_ -> Check

(* 

let chen_strength (in_hand:card list) : int= 
  let highest = List.hd (values_in_hand (highest_n 1 in_hand [])) in
  let highest_val = if highest = 12 then float_of_int 10 else if highest = 11 then float_of_int 8
    else if highest = 10 then float_of_int 7 else if highest = 9 then float_of_int 6 else
      (float_of_int highest)/.2. in 
  let suited = if List.hd (suits_in_hand in_hand) = List.hd (List.tl (suits_in_hand in_hand))
    then true else false in
  let pair = if (pairs in_hand []) <> [] then true else false in
  let gap = 
    if (pairs in_hand []) <> [] then if highest > 10 then 10 
      else if highest > 8 then 9 else if highest > 5 then 8 else 7
    else if List.hd (suits_in_hand in_hand) = List.hd (List.tl (suits_in_hand in_hand)) then
      if highest > 10 then 7 else if highest > 8 then 6 else if highest > 5 then 5 else 3
    else if List.hd (values_in_hand in_hand) = List.hd (List.tl (values_in_hand in_hand))+1
         ||List.hd (values_in_hand in_hand) = List.hd (List.tl (values_in_hand in_hand))-1 then
      if highest > 10 then 7 else if highest > 8 then 6 else if highest > 5 then 5 else 3
    else if highest > 10 then 6 else if highest > 8 then 4 else if highest > 6 then 3 else
    if highest > 4 then 2 else 1 *)

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
