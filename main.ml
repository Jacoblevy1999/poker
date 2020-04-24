open Poker
open State 
open Command

(** [remove_dsign str] takes a string of the form of a dollar sign ($) followed
    by an integer and returns the integer as type int. 
    Example: [remove_dsign "$25"] returns [25]. *)
let remove_dsign str = 
  let list = String.split_on_char '$' str in 
  match list with 
  |""::[t] -> int_of_string t 
  |_ -> failwith "Not the right form"

let rec loop state : unit = 
  if state.stage = 0 then loop (deal state) else
    let move = read_line () in
    let command = try (Command.parse move) with Invalid_move -> (Loop) in
    match command with
    |Call -> loop (call state)
    |Check -> loop (check state)
    |Fold -> loop (fold state)
    |Bet amount -> if amount<>(-1) then loop (bet state amount) else (print_endline "How much do you want to bet?"); loop state
    |Raise amount -> if amount<>(-1) then loop (raise state amount) else (print_endline "How much do you want to raise?"); loop state
    |Buy_in amount -> if amount<>(-1) then loop ({ hand1 = Array.of_list ([]) ; hand2 = Array.of_list ([]) ; 
                                                   table = Array.of_list ([]) ; cards = state.cards ; cash1 = (state.cash1 + amount) ; 
                                                   cash2 = (state.cash2 + amount) ; pot = state.pot ; ante = state.ante ; previous_bet = 0 ; turn = state.started
                                                 ; started = state.started ; stage = 0} ) else  (print_endline "How much do you want to buy in?"); loop state
    |Help -> print_endline "To place a bet, type 'bet [amount]', i.e. 'bet $25' bets $25.";
      print_endline "To check, type 'check'.";
      print_endline "To raise a bet, type 'raise [amount]', i.e. 'riase $30' will raise the bet by $30.";
      print_endline "To call, type 'call'.";
      print_endline "To fold, type 'fold'.";
      print_endline "To buy in, type 'buy in [amount]', i.e. 'buy in $50' will add $50 to your cash.";
      print_endline "To quit, type 'quit'.";
      print_endline "To see this list of commands again, type 'help'.";
    |Quit -> print_endline "Thanks for playing!" ; exit 0
    |Loop -> loop state

let play_game =
  print_endline "How much money is each player gambling with? 
  Type an amount between $10 and $100000 and then hit enter.";
  print_string  "> ";
  let buy_in_str = read_line () in 
  let buy_in_int = remove_dsign buy_in_str in 
  print_endline "How much is the ante? Enter an amount between $1 and $1000.";
  print_string "> ";
  let ante_str = read_line () in 
  let ante_int = remove_dsign ante_str in 
  let init = State.init_state buy_in_int buy_in_int ante_int 1 in 
  print_endline "To place a bet, type 'bet [amount]', i.e. 'bet $25' bets $25.";
  print_endline "To check, type 'check'.";
  print_endline "To raise a bet, type 'raise [amount]', i.e. 'riase $30' will raise the bet by $30.";
  print_endline "To call, type 'call'.";
  print_endline "To fold, type 'fold'.";
  print_endline "To buy in, type 'buy in [amount]', i.e. 'buy in $50' will add $50 to your cash.";
  print_endline "To quit, type 'quit'.";
  print_endline "To see this list of commands again, type 'help'.";
  print_endline "Player 1 starts. Enjoy the game!";
  loop init

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  print_endline
    "\n\nWelcome, are you ready to take a seat at the table and 
                  play heads up poker?\n";
  print_endline "Type 'play' and then press enter when you're ready to start.\n";
  print_string  "> ";
  match read_line () with
  |"play" -> play_game
  |"play." -> play_game
  |"Play" -> play_game
  |"Play." -> play_game
  | _ -> main ()


(* Execute the game engine. *)
let () = main ()