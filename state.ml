(**Open Poker **)
open Poker

(**list of player 1's 2 card hand **)
type player1_hand = card array

(**list of player 2;s 2 card hand **)
type player2_hand = card array

(**list of cards dealt on table **)
type table = card array

(**list of 9 cards pulled from poker.deal that will be dealt, is a mutable
   array type so you can pull it out directly and will change**)
type cards = card array

(**value of player 1's cash **)
type player1_cash = int

(**value of player 2's cash **)
type player2_cash = int

(**value of pot **)
type pot = int

(**pre-determined ante amount from poker customization **)
type ante = int

(**indicates which player's turn it is, [1] for player 1 and [-1] for player 2,
   can only ever be [1] or [-1]**)
type turn = int

(**indicates which player started the hand, [1] for player 1 and [-1] for 
   player 2, only ever changed when initialized**)
type started = int 


type t = { hand1 : player1_hand ; hand2 : player2_hand ; 
           table : table ; cards : cards ; cash1 : player1_cash ; 
           cash2 : player2_cash ; pot : pot ; ante : ante ; turn : turn
         ; started : started} 


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
    cash2 = cash2 ; pot = 0 ; ante = ante ; turn = started
  ; started = started} 

let hand1 st = 
  st.hand1

let hand2 st = 
  st.hand2

let table st =
  st.table

(**usually main should not be able to access this, just a helper function **)
let cash1 st =
  st.cash1

let cash2 st =
  st.cash2

let pot st = 
  st.pot


(**BEGINNING OF NON TURN BASED STATE FUNCTIONS ex deal, flop, turn river **)


(**deals 2 cards to each player by taking off first 4 elements of 
   array st.cards, then auto bets the ante of each player.  **)
let deal st = 

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
    cash1 = st.cash1 ; 
    cash2 = st.cash2 ; 

    (**pot stays at 0 after deal **)
    pot = 0 ; 

    (**ante stays the same after deal **)
    ante = st.ante ; 

    (**turn stays the same because dealing doesn't 
       count as a move for player **)
    turn = st.turn ;

    (**started stays consistent entire time unless initializing new state **)
    started = st.started
  }

let flop st = 
  (**have assert statement to confirm length of array st.cards is 5 if fails
     then this action of flop is not possible **)
  assert (Array.length st.cards == 5);

  let flop = (Poker.flop (st.cards) (Array.of_list ([]))) in 
  let remainingcards = (fst flop) in 
  let table = (snd flop) in 

  (**table gets reassigned, st.cards gets reassigned, everything else
     stays untouched because no turn is made **)
  { hand1 = st.hand1 ; hand2 = st.hand2 ; 
    table = table ; cards = remainingcards ; cash1 = st.cash1 ; 
    cash2 = st.cash2 ; pot = st.pot
  ; ante = st.ante ; turn = st.turn
  ; started = st.started} 


let turn st = 

  (**checks to make sure turn is a valid move **)
  assert (Array.length st.cards == 2);

  let turn = (Poker.turn (st.cards) (st.table)) in 
  let remainingcards = (fst turn) in 
  let table = (snd turn) in

  { hand1 = st.hand1 ; hand2 = st.hand2 ; 
    table = table ; cards = remainingcards ; cash1 = st.cash1 ; 
    cash2 = st.cash2 ; pot = st.pot
  ; ante = st.ante ; turn = st.turn
  ; started = st.started} 

let river st = 
  (**checks to make sure turn is a valid move **)
  assert (Array.length st.cards == 1);
  let table = (Poker.river (st.cards) (st.table)) in 

  { hand1 = st.hand1 ; hand2 = st.hand2 ; 
    table = table ; cards = Array.of_list ([]) ; cash1 = st.cash1 ; 
    cash2 = st.cash2 ; pot = st.pot
  ; ante = st.ante ; turn = st.turn
  ; started = st.started} 


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
        turn = -(st.turn) ; started = st.started} 

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
        turn = -(st.turn) ; started = st.started)} 

else failwith "Not enough cash"

| _ -> failwith "Not possible"

(**check does not change anything, just moves the turn counter **)
let check st = 
  { hand1 = st.hand1 ; hand2 = st.hand2 ; 
    table = st.table ; cards = st.cards ; cash1 = st.cash1 ; 
    cash2 = st.cash2 ; pot = st.pot
  ; ante = st.ante ; turn = -(st.turn)
  ; started = st.started} 

(**declares winner between two hands and all table hands and declares winner 
   by moving pot over and resetting hands **)
let winner st = 
  failwith "Unimplemented"






(**END OF TURN BASED FUNCTIONS **)













