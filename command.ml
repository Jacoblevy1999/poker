open Poker 

type move_phrase = string list

type move = 
  (* Calling is matching the amount that has been put in by another player in the form of a bet or a raise. *)
  |Call
  (* Checking is what one does if they wish to pass the action to the next player, but keep their cards. *)
  |Check
  (* Folding is not betting and discarding cards, out till next hand. *)
  |Fold 
  (* Betting is the opening bet of a hand. *)
  |Bet of int
  (* Raising is betting more than the previous bet on the table. *)
  |Raise of int
  |Buy_in of int
  |Help
  |Quit
  |Loop



exception Empty 
exception Invalid_move

(* [parse_helper str] returns the list of strings with all spaces and empty 
   strings removed. The list is in the same order as the original string.
   Requires: strlst is a list of strings *)
let str_to_strlst_helper strlst = 
  let rec aux acc = function
    |[] -> List.rev acc
    |h::t -> if h="" || h=" " then aux acc t  else aux (h::acc) t
  in aux [] strlst

(* [str_to_strlst str] splits a string at the spaces then returns a lsit of 
   strings with all spaces and empty strings removed.
   Requires: str is a string.   *)
let str_to_strlst str = 
  let str_lst = String.split_on_char ' ' str in
  str_to_strlst_helper str_lst

(** is_phrase_wellformed move_phrase] returns true if [move_phrase] 
    satisfies the ctriteria given below, raises an error otherwise. 
    Criteria: Each element of the list represents a word of the move 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.

    An [move_phrase] is not permitted to be the empty list. *)
let rec is_phrase_wellformed strlst = 
  if (List.length strlst = 0) then raise(Empty) else 
    match strlst with 
    |[] -> true
    |h::t -> if h = " " || h = "" then raise(Invalid_move) else true 

let parse str = 
  let move = str_to_strlst str in
  match move with 
  |[] -> raise(Empty)
  |h::t -> 
    match h, t with 
    |"fold", [] -> Fold
    |"call", [] -> Call
    |"check", [] -> Check 
    |"raise", [] -> Raise (-1)
    |"raise", [bet] -> Raise (int_of_string bet)
    |"bet", [bet] -> Bet (int_of_string bet)
    |"quit", [] -> Quit
    |"buy", "in"::[] -> Buy_in (-1)
    |"buy", "in"::[amount] -> Buy_in (int_of_string amount)
    |"help", [] -> Help
    |_, _ -> raise(Invalid_move)



