(**Open Poker **)
open poker

(**list of player 1's 2 card hand **)
type player1_hand = card list

(**list of player 2;s 2 card hand **)
type player2_hand = card list 

(**list of cards dealt on table **)
type table = card list

(**list of 9 cards pulled from poker.deal that will be dealt, is a mutable
   array type so you can pull it out directly and will change**)
type cards = card array

(**value of player 1's cash **)
type player1_cash = int

(**value of player 2's cash **)
type player2_cash = int

(**value of pot **)
type pot = int


type t = { player1_hand : player1_hand ; player2_hand : player2_hand ; 
           table : table ; cards : cards ; player1_cash : player1_cash ; 
           player2_cash : player2_cash ; pot : pot} 


(**initializes type state, always called when starting new hand **)
let init_state hand1 hand2 table cards cash1 cash2 pot = 

  (**state is initialized but cards are assigned to random array list of 9 
     cards called from Poker.shuffle **)
  { player1_hand : hand1 ; player2_hand : hand2 ; 
    table : table ; cards : Poker.shuffle ; player1_cash : cash1 ; 
    player2_cash : cash2 ; pot : 0} 

let hand1 st = 
  st.player1_hand

let hand2 st = 
  st.player2_hand

let table st =
  st.table

(**usually main should not be able to access this, just a helper function **)
let cards st = 
  st.cards

let player1_cash st =
  st.player1_cash

let player2_cash st =
  st.player2_cash

let pot st = 
  st.pot













