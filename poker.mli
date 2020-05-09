(** The Poker module does three key things: it builds functions to be used in
    dealing cards, it evaluates the winner of a hand, and it prints cards in 
    a desired way. *)

(** [card] is the representation of a card. *)
type card

(** [deck] is an array of cards in the deck. *)
type deck

(** [hand] is an array of cards in a player's hand. *)
type hand

(** [table] is the array of cards on the table. *)
type table

(** [allseven] is a list of cards representing those both in a player's hand,
    as well as those on the table. *)
type allseven

(** [full_deck] represents a full [deck] of playing cards. *)
val full_deck : deck

(** [shuffle fulldeck acc] is 9 cards randomly selected from [fulldeck]. *)
val shuffle : deck -> deck -> deck

(** [deal deck hand] is a tuple representing the cards remaining in [deck],
    and the 2 cards in a player's [hand] after being dealt. *)
val deal : deck -> hand -> deck * hand

(** [flop deck table] is a tuple is a tuple representing the cards remaining in [deck],
    and the cards on the [table] after the flop. *)
val flop : deck -> table -> deck * table

(** [turn deck table] is a tuple is a tuple representing the cards remaining in [deck],
    and the cards on the [table] after the turn. *)
val turn : deck -> table -> deck * table

(** [river deck table] is the cards on the [table] after the river. *)
val river : deck -> table -> table

(** [winner h1 h2 t] is "player 1" is h1 is the stronger hand, "player 2" if h2
    is the stronger hand, and "tie" if they are equally strong, given [t] is the
    cards on the table and [h1] and [h2] and the two cards in each player's hand. *)
val winner : hand -> hand -> table -> string

