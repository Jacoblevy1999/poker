open Poker
open State 
open Command

let rec loop (st) (res) (deck): unit = 
  if ((player1_cash < ante)||(player2_cash < ante)) then (print_endline "You can't afford the ante, you need to buy in!";
  else print_string " ";
  let move = read_line () in
  let command = try (Command.parse reply) with Malformed -> (Loop) in
  match command with
  |Call -> update_state
  |Check -> update_state
  |Fold -> update_state
  |Bet amount -> if amount=(-1) then (print_endline "How much do you want to bet?";) repeat else update_state
  |Raise amount -> if amount=(-1) then (print_endline "How much do you want to raise?";) repeat else update_state
  |Buy_in amount -> if amount=(-1) then (print_endline "How much do you want to buy in?";) repeat else update_state 
  |Help -> print_endline "To place a bet, type 'bet [amount]', i.e. 'bet $25' bets $25.";
    print_endline "To raise a bet, type 'raise [amount]', i.e. 'riase $30' will raise the bet to $30.";
    print_endline "To call, type 'call'.";
    print_endline "To check, type 'check'.";
    print_endline "To fold, type 'fold'.";
    print_endline "To buy in, type 'buy in [amount]', i.e. 'buy in $50' will add $50 to your cash.";
    print_endline "To quit, type 'quit'.";
    print_endline "To see this list of commands again, type 'help'.";
  |Quit -> print_endline "Thanks for playing!" ; loop st res adv
  |Loop -> repeat

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let adv = Adventure.from_json (Yojson.Basic.from_file f) in
  let init_state = init_state adv in
  let init_result = State.initial_go adv init_state in 
  print_endline (get_room_description init_result adv);
  loop init_state init_result adv

let play_game f =
  let init_state = init_state adv in

  let init_result = State.initial_go adv init_state in 
  print_endline (get_room_description init_result adv);
  loop init_state init_result adv 

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name


(* Execute the game engine. *)
let () = main ()