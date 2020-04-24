(**change cards to deck **)
(**comments 

   Ability to buy back in when not enough 

   Add a element of state that just says what round it is
   ex, after 

   Maybe do a multi pronged state function - step through those elements **)

(**Open Poker **)
open Poker

(**list of player 1's 2 card hand **)
type hand1 = card array

(**list of player 2;s 2 card hand **)
type hand2 = card array

(**list of cards dealt on table **)
type table = card array

(**list of 9 cards pulled from poker.deal that will be dealt, is a mutable
   array type so you can pull it out directly and will change**)
type cards = card array

(**value of player 1's cash **)
type cash1 = int

(**value of player 2's cash **)
type cash2 = int

(**value of pot **)
type pot = int

(**pre-determined ante amount from poker customization **)
type ante = int

(**indicates previous amount bet or raised, only used when checking if check 
   or raise are valid functions **)
type previous_bet = int

(**indicates which player's turn it is, [1] for player 1 and [-1] for player 2,
   can only ever be [1] or [-1]**)
type turn = int

(**indicates which player started the hand, [1] for player 1 and [-1] for 
   player 2, only ever changed when initialized new hand with init_state**)
type started = int

(**indicates which stage of the game state is currently at 

   Limited to [0,1,2,3,4]

   Pre-deal stage = 0
   Post-deal stage = 1
   Post-flop stage = 2
   Post-turn stage = 3
   Post-river stage = 4

 **)
type stage = int

type t = { hand1 : hand1 ; hand2 : hand2 ; 
           table : table ; cards : cards ; cash1 : cash1 ; 
           cash2 : cash2 ; pot : pot ; ante : ante ; 
           previous_bet : previous_bet ; turn : turn
         ; started : started ; stage : stage} 


(**initializes type state, always called when starting new hand therefore the 
   only information passing from one round to another is the amount of
   cash each player has left and their ante**)

(**started is whoever started prior, from main access this as State.started 
   where you pull out who started on prior hand, and state_init will flip that **)
let init_state cash1 cash2 ante started = 

  let cards = Poker.shuffle (Poker.full_deck) (Array.of_list ([])) in 

  (**state is initialized but cards are assigned to random array list of 9 
     cards called from Poker.shuffle **)
  { hand1 = Array.of_list ([]) ; hand2 = Array.of_list ([]) ; 
    table = Array.of_list ([]) ; cards = cards ; cash1 = cash1 ; 
    cash2 = cash2 ; pot = 0 ; ante = ante ; previous_bet = 0 ; turn = started
  ; started = started ; stage = 0} 


(**returns player 1 hand **)
let hand1 st = 
  st.hand1

(**returns plauer 2 hand **)
let hand2 st = 
  st.hand2

(**returns cards out on table **)
let table st =
  st.table

(**returns player 1 cash **)
let cash1 st =
  st.cash1

(**returns player 2 cash  **)
let cash2 st =
  st.cash2

(**returns value of pot of that given hand**)
let pot st = 
  st.pot

(**returns hand of player whose current turn it is, helper function for main **)
let current_hand st = 
  match st.turn with 
  | 1 -> st.hand1
  | -1 -> st.hand2
  | _ -> failwith "Not possible"

(**BEGINNING OF NON TURN BASED STATE FUNCTIONS ex deal, flop, turn river 
   winner **)


(**deals 2 cards to each player by taking off first 4 elements of 
   array st.cards, then auto bets the ante of each player.  **)
let deal st = 

  assert (Array.length st.cards == 9);

  (**both players must have more cash than the ante before dealing, otherwise
     you cannot deal and player must buy in for more **)
  assert (st.cash1 > st.ante);
  assert (st.cash2 > st.ante);

  let hand1 = snd (Poker.deal (st.cards) (Array.of_list ([]))) in 
  let remaining1 = fst (Poker.deal (st.cards) (Array.of_list ([]))) in 
  let hand2 = snd (Poker.deal (remaining1) (Array.of_list ([]))) in 
  let remainingcards = fst (Poker.deal (remaining1) (Array.of_list ([]))) in 
  {
    (**returns hand1 as list of 2 cards **)
    hand1 = hand1 ; 

    (**returns hand2 as list of 2 cards **)
    hand2 = hand2 ; 

    (**during deal nothing put on table, stay as empty list **)
    table = Array.of_list ([]) ; 

    (**remaining cards are whats left after dealing both hands **)
    cards = remainingcards ; 

    (**player cash does not change during deal **)
    cash1 = st.cash1 - st.ante ; 
    cash2 = st.cash2 - st.ante; 

    (**pot stays at 0 after deal **)
    pot = (2 * st.ante) ; 

    (**ante stays the same after deal **)
    ante = st.ante ; 

    (**no previous bet so does not affect previous_bet **)
    previous_bet = 0;

    (**turn stays the same because dealing doesn't 
       count as a move for player **)
    turn = st.turn ;

    (**started stays consistent entire time unless initializing new state **)
    started = st.started ;

    (**updates to post-deal stage **)
    stage = 1
  }

let flop st = 
  (**have assert statement to confirm length of array st.cards is 5 if fails
     then this action of flop is not possible **)
  assert (Array.length st.cards == 5);

  let flop = (Poker.flop (st.cards) (Array.of_list ([]))) in 
  let remainingcards = (fst flop) in 
  let table = (snd flop) in 
  print_string "The cards on the table are "; print_endline (Array.to_list table |> format_lst);

  (**table gets reassigned, st.cards gets reassigned, everything else
     stays untouched because no turn is made **)
  { hand1 = st.hand1 ; hand2 = st.hand2 ; 
    table = table ; cards = remainingcards ; cash1 = st.cash1 ; 
    cash2 = st.cash2 ; pot = st.pot
  ; ante = st.ante ; previous_bet = 0 ; turn = st.started
  ; started = st.started ; stage = 2} 


let turn st = 

  (**checks to make sure turn is a valid move **)
  assert (Array.length st.cards == 2);

  let turn = (Poker.turn (st.cards) (st.table)) in 
  let remainingcards = (fst turn) in 
  let table = (snd turn) in
  print_string "The cards on the table are "; print_endline (Array.to_list table |> format_lst);
  { hand1 = st.hand1 ; hand2 = st.hand2 ; 
    table = table ; cards = remainingcards ; cash1 = st.cash1 ; 
    cash2 = st.cash2 ; pot = st.pot
  ; ante = st.ante ; previous_bet = 0 ; turn = st.started
  ; started = st.started ; stage = 3} 

let river st = 
  (**checks to make sure turn is a valid move **)
  assert (Array.length st.cards == 1);
  let table = (Poker.river (st.cards) (st.table)) in 
  print_string "The cards on the table are "; print_endline (Array.to_list table |> format_lst);
  { hand1 = st.hand1 ; hand2 = st.hand2 ; 
    table = table ; cards = Array.of_list ([]) ; cash1 = st.cash1 ; 
    cash2 = st.cash2 ; pot = st.pot
  ; ante = st.ante ; previous_bet = 0 ; turn = st.started
  ; started = st.started ; stage = 4} 


(**declares winner between two hands and all table hands and declares winner 
   by moving pot over and resetting hands, not a turn based function
   because it takes in an input state [st] and spits out an init_state w/
   winner of hand taking previous pot and resetting of hands

   st.started also changes as the previous starter switches over **)
let winner st = 

  (**check winner check is appropriate **)
  assert (Array.length st.table == 5);
  assert (Array.length st.cards == 0);

  let hand1 = st.hand1 in 
  let hand2 = st.hand2 in 
  let winner = Poker.winner hand1 hand2 (st.table) in 
  match winner with 

  (**if player 1 wins then reassign pots and reset game accordingly **)
  |"player 1" -> 

    init_state (st.cash1 + st.pot) (st.cash2) (st.ante) (-(st.started))

  (**if player 2 wins then reassign pots and reset game accordingly **)
  |"player 2" ->

    init_state (st.cash1) (st.cash2 + st.pot) (st.ante) (-(st.started))

  (**if players tie then reassign pots and reset game accordingly **)
  |"tie" ->
    let payout = (st.pot/2) in

    init_state (st.cash1 + payout) (st.cash2 + payout) (st.ante) (-(st.started))

  | _ -> failwith "Not Possible"


(**END OF NON TURN BASED FUNCTIONS **)


(*BEGINNING OF TURN BASED FUNCTIONS **)


(**takes state and just creates a new initialized state with empty hands and 
   the pot distributed to winner, fold dependent on player turn**)
let fold st = 

  (**check who folds dependent on that is who you change the pot value **)
  if (st.turn = 1) 

  (**if player 1 turn, then intialize new hand with player 1 cash
     same and player 2 gaining pot value **)
  then init_state (st.cash1) (st.cash2 + st.pot) (st.ante) (-(st.started))

  (**if player 2 turn, then initialize new hand with player 2 cash same
     and player 1 gaining pot value **)
  else init_state (st.cash1 + st.pot) (st.cash2) (st.ante) (-(st.started))


(**takes in state and an amount and bets it dependent on player turn **)
let bet st amt = 

  match st.turn with 
  | 1 -> if (amt < st.cash1)
    then
      { 
        (**hands table cards and randomized cards stay same **)
        hand1 = st.hand1 ; hand2 = st.hand2 ; 
        table = st.table ; cards = st.cards ; 


        (**if player1 turn then bet amount subtracted from player1_cash **)
        cash1 = (st.cash1 - amt) ; 
        cash2 = st.cash2 ; pot = (st.pot + amt) ; ante = st.ante ; 

        (**assign previous bet to amt value to track in case of following
           check or re-raise action **)
        previous_bet = amt ;

        (**alternate turn, stage, starting stays same **)
        turn = -(st.turn) ; started = st.started ; stage = st.stage} 

    else failwith "Not enough cash"


  | -1 -> if (amt < st.cash2)
    then
      { 
        (**hands table cards and randomized cards stay same **)
        hand1 = st.hand1 ; hand2 = st.hand2 ; 
        table = st.table ; cards = st.cards ; 


        (**if player2 turn then bet amount subtracted from st.cash2 **)
        cash1 = (st.cash1) ; 
        cash2 = (st.cash2-amt) ; pot = (st.pot + amt) ; ante = st.ante ; 

        (**assign previous bet to amt value to track in case of following
           check or re-raise action **)
        previous_bet = amt ;

        turn = -(st.turn) ; started = st.started ; stage = st.stage} 

    else failwith "Not enough cash"

  | _ -> failwith "Not possible"

(**inputs state where player just bet and sees if it is valid to raise specific
   amount according to rule that raises are atleast 2*previous_bet **)
let raise st amt = 

  (**checks to see if another bet was made in prior action **)
  assert (st.previous_bet > 0);

  (**checks to see if raise amount is atleast twice the previous bet **)
  assert (amt > 2*st.previous_bet);

  match st.turn with 
  | 1 -> 

    (**Check if player1 has enough cash1 to raise the previous_bet **)
    assert (amt < st.cash1) ;

    { 
      (**hands table cards and randomized cards stay same **)
      hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = st.table ; cards = st.cards ; 


      (**if player1 turn then raise amount subtracted from cash1 **)
      cash1 = (st.cash1 - amt) ; 
      cash2 = st.cash2 ; pot = (st.pot + amt) ; ante = st.ante ; 

      (**assign previous raise amt value to track in case of following
         check or re-raise action **)
      previous_bet = amt ;

      (**alternate turn, starting stays same **)
      turn = -(st.turn) ; started = st.started ; stage = st.stage} 
  | -1 -> 

    (**Check if player1 has enough cash2 to raise the previous_bet**)
    assert (amt < st.cash2) ;

    { 
      (**hands table cards and randomized cards stay same **)
      hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = st.table ; cards = st.cards ; 


      (**if player1 turn then previous_bet amount subtracted from cash1 **)
      cash1 = (st.cash1) ; 
      cash2 = st.cash2 - amt ; pot = (st.pot + amt) ; ante = st.ante ; 

      (**assign previous bet to amt value to track in case of following
         check or re-raise action **)
      previous_bet = amt ;

      (**alternate turn, starting stays same **)
      turn = -(st.turn) ; started = st.started ; stage = st.stage} 

  | _ -> failwith "Invalid Command"


(**inputs state where player just bet/raised, checks to see if valid to call
   here if it does then returns new state with call, else does not return a 
   new state and says invalid command **)
let call st = 

  (**checks to see if another bet was made in prior action **)
  assert (st.previous_bet > 0);

  match st.turn with 
  | 1 -> 

    (**failsafe to check if player1 has enough cash1 to call the previous_bet **)
    assert (st.previous_bet < st.cash1) ;

    { 
      (**hands table cards and randomized cards stay same **)
      hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = st.table ; cards = st.cards ; 


      (**if player1 turn then previous_bet amount subtracted from cash1 **)
      cash1 = (st.cash1 - st.previous_bet) ; 
      cash2 = st.cash2 ; pot = (st.pot + st.previous_bet) ; ante = st.ante ; 

      (**since you cannot call or raise a call previous_bet gets 
         re-assigned to 0 **)
      previous_bet = 0 ;

      (**alternate turn, starting stays same **)
      turn = -(st.turn) ; started = st.started ; stage = st.stage} 
  | -1 -> 

    (**failsafe to check if player1 has enough cash1 to call the previous_bet **)
    assert (st.previous_bet < st.cash2) ;

    { 
      (**hands table cards and randomized cards stay same **)
      hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = st.table ; cards = st.cards ; 


      (**if player1 turn then previous_bet amount subtracted from cash1 **)
      cash1 = (st.cash1 - st.previous_bet) ; 
      cash2 = st.cash2 ; pot = (st.pot + st.previous_bet) ; ante = st.ante ; 

      (**assign previous bet to amt value to track in case of following
         check or re-raise action **)
      previous_bet = 0 ;

      (**alternate turn, starting stays same **)
      turn = -(st.turn) ; started = st.started ; stage = st.stage} 

  | _ -> failwith "Invalid Command"

(**check does not change anything, just moves the turn counter **)
let check st = 

  { hand1 = st.hand1 ; hand2 = st.hand2 ; 
    table = st.table ; cards = st.cards ; cash1 = st.cash1 ; 
    cash2 = st.cash2 ; pot = st.pot
  ; ante = st.ante ; previous_bet = 0 ; turn = -(st.turn)
  ; started = st.started ; stage = st.stage} 

(**END OF TURN BASED FUNCTIONS **)













