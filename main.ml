open Poker
open State 
open Command



let whose_turn state = 
  if state.turn = 1 then print_string "It is player 1's turn. > "
  else print_string "It is player 2's turn. > "

(**if each player has enough cash, then this is looped back into state, if not
   then it recurses until player buys in for enough **)

(**returns a dealt hand after ante is paid, else it recurses until ante is paid **)
let rec ante_check state = 
  whose_turn state;
  let move = read_line () in
  let command = try (Command.parse move) with Invalid_move -> (print_endline "Invalid move"; Loop) in
  match command with 
  | Buy_in amount -> (buyin state amount) 
  |Quit -> print_endline "Thanks for playing!" ; exit 0
  |Cash1 -> print_endline ("$"^string_of_int (state.cash1)); ante_check state
  |Cash2 -> print_endline ("$"^string_of_int (state.cash2)); ante_check state
  | _ -> print_endline "Please buy in for more before countinuing by typing 'buy in [x]' or exit the game by typing 'quit'" ; ante_check state

let rec loop state : unit = 

  (**checks if in pre-deal stage **)
  if state.stage == 0 
  then (

    if (state.cash1 < state.ante || state.cash2  < state.ante) then 

      match state.cash1 < state.ante with 

      (**means player 1 does not have enough for ante **)
      | true -> print_endline ("\nPlayer 1 does not have enough to pay the ante of $"^  string_of_int state.ante ^ ". You currently have $" ^ string_of_int state.cash1 ^ ". You must buy in for atleast $" ^ string_of_int (state.ante - state.cash1) ^ " more.\n");

        (**if its player 1 turn then just buy in for more and move on **)
        if (state.turn == 1)
        then loop (ante_check state)
        (**if it is not player 1 turn, then we make it player 1 turn he buys in, and we return back to original turn *)
        else let tempstate = check state in 
          let antestate = ante_check tempstate in 
          let finalstate = check antestate in 
          loop finalstate

      (**means player 2 does not have enough for ante **)
      | false ->  print_endline ("\nPlayer 2 does not have enough to pay the ante of $"^  string_of_int state.ante ^ ". You currently have $" ^ string_of_int state.cash2 ^ ". You must buy in for atleast $" ^ string_of_int (state.ante - state.cash2) ^ " more.\n");
        (**if its player 1 turn then just buy in for more and move on **)
        if (state.turn == -1)
        then loop (ante_check state)
        (**if it is not player 1 turn, then we make it player 1 turn he buys in, and we return back to original turn *)
        else let tempstate = check state in 
          let antestate = ante_check tempstate in 
          let finalstate = check antestate in 
          loop finalstate

    else loop (deal state)
  )

  else 

    whose_turn state;
  let move = read_line () in
  let command = try (Command.parse move) with Invalid_move -> (print_endline "Invalid move. Enter 'help' to see the list of moves."; Loop) in
  match command with
  |Call -> print_endline ""; loop (new_cards (state) Call)
  |Check -> print_endline ""; loop (new_cards state Check)
  |Fold -> print_endline ""; loop (fold state)
  |Bet amount -> if amount<>(-1) then loop (bet state amount) else (print_endline "How much do you want to bet?"); loop state
  |Raise amount -> if amount<>(-1) then loop (raise state amount) else (print_endline "How much do you want to raise?"); loop state
  |Buy_in amount -> if amount<>(-1) then loop (buyin state amount)
    else  (print_endline "How much do you want to buy in?"); loop state
  |Help -> print_endline "To place a bet, type 'bet [amount]', i.e. 'bet $25' bets $25.";
    print_endline "To check, type 'check'.";
    print_endline "To raise a bet, type 'raise [amount]', i.e. 'raise $30' will raise the bet to $30.";
    print_endline "To call, type 'call'.";
    print_endline "To fold, type 'fold'.";
    print_endline "To see player 1's cards, enter 'p1 cards'";
    print_endline "To see player 2's cards, enter 'p2 cards'";
    print_endline "To see the money in the pot, enter 'pot'.";
    print_endline "To buy in, type 'buy in [amount]', i.e. 'buy in $50' will add $50 to your cash.";
    print_endline "To quit, type 'quit'.";
    print_endline "To see this list of commands again, type 'help'.";
    loop state
  |Cards1 -> if state.turn <> 1 then (print_endline "You can't look at player 1's cards!"; (loop state);) else print_endline (Array.to_list (state.hand1) |> hand_to_input |> pp2); print_endline "Enter 'clear' to hide your cards."; loop state
  |Cards2 -> if state.turn <> -1 then (print_endline "You can't look at player 2's cards!"; (loop state);) else print_endline (Array.to_list (state.hand2) |> hand_to_input |> pp2); print_endline "Enter 'clear' to hide your cards."; loop state
  |Cash1 -> print_endline ("$"^string_of_int (state.cash1)); loop state
  |Cash2 -> print_endline ("$"^string_of_int (state.cash2)); loop state
  |Pot -> print_endline ("$"^string_of_int (state.pot)); loop state
  |Quit -> print_endline "Thanks for playing!" ; exit 0
  |Clear -> let _ = Sys.command("clear") in 
    if state.stage = 0 || state.stage = 1 then (print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; loop state)
    else if state.stage = 2 then (print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|";(print_endline "The cards on the table are:"); (print_endline (Array.to_list (state.table) |> hand_to_input |> pp3)); loop state)
    else if state.stage = 3 then (print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|";(print_endline "The cards on the table are:"); (print_endline (Array.to_list (state.table) |> hand_to_input |> pp4)); loop state)
    else if state.stage = 4 then (print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|"; print_endline "|";(print_endline "The cards on the table are:"); (print_endline (Array.to_list (state.table) |> hand_to_input |> pp5)); loop state)
  |Loop -> loop state

let rec set_buy_in str = 
  print_endline "Type an amount between $10 and $1000000 and then hit enter.";
  print_string  "> ";
  let buy_in_str = read_line () in 
  if buy_in_str = "quit" then begin print_endline "Thanks for playing!" ; exit 0 end
  else let buy_int_int = remove_dsign buy_in_str in
    if buy_int_int = (-1) then set_buy_in "" else 
    if buy_int_int > 9 && buy_int_int < 1000001 then buy_int_int 
    else set_buy_in ""

let rec set_ante str = 
  print_endline "Type an amount between $1 and $1000 and then hit enter.";
  print_string  "> ";
  let ante_str = read_line () in 
  if ante_str = "quit" then begin print_endline "Thanks for playing!" ; exit 0 end
  else let ante_int = remove_dsign ante_str in
    if ante_int = (-1) then set_ante "" else 
    if ante_int > 0 && ante_int < 1001 then ante_int
    else set_ante ""

let play_game =
  print_endline
    "\n\nWelcome, are you ready to take a seat at the table and 
  play heads up poker?\n";
  print_endline "How much money is each player gambling with?"; 
  let buy_in_int = set_buy_in "" in
  print_endline "How much is the ante?";
  let ante_int = set_ante "" in
  let init = State.init_state buy_in_int buy_in_int ante_int 1 in 
  print_endline "To place a bet, type 'bet [amount]', i.e. 'bet $25' bets $25.";
  print_endline "To check, type 'check'.";
  print_endline "To raise a bet, type 'raise [amount]', i.e. 'riase $30' will raise the bet by $30.";
  print_endline "To call, type 'call'.";
  print_endline "To fold, type 'fold'.";
  print_endline "To see player 1's cards, enter 'p1 cards'";
  print_endline "To see player 2's cards, enter 'p2 cards'";
  print_endline "To see the money in the pot, enter 'pot'.";
  print_endline "To see player 1's cash, enter 'p1 cash'";
  print_endline "To see player 2's cash, enter 'p2 cash'";
  print_endline "To buy in, type 'buy in [amount]', i.e. 'buy in $50' will add $50 to your cash.";
  print_endline "To quit, type 'quit'.";
  print_endline "To see this list of commands again, type 'help'.";
  print_endline "Player 1 starts. Enjoy the game!";
  loop (init)

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
