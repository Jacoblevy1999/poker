type suit =
  |Hearts
  |Diamonds
  |Clubs
  |Spades

type card = 
  |Two of suit  
  |Three of suit
  |Four of suit
  |Five of suit
  |Six of suit
  |Seven of suit
  |Eight of suit
  |Nine of suit
  |Ten of suit
  |Jack of suit
  |Queen of suit
  |King of suit
  |Ace of suit

type deck = card array
type hand = card array

let full_deck = Array.of_list [Two Hearts; Two Diamonds; Two Clubs; Two Spades;
                               Three Hearts; Three Diamonds; Three Clubs; Three Spades; 
                               Four Hearts; Four Diamonds; Four Clubs; Four Spades;
                               Five Hearts; Five Diamonds; Five Clubs; Five Spades;
                               Six Hearts; Six Diamonds; Six Clubs; Six Spades;
                               Seven Hearts; Seven Diamonds; Seven Clubs; Seven Spades;
                               Eight Hearts; Eight Diamonds; Eight Clubs; Eight Spades;
                               Nine Hearts; Nine Diamonds; Nine Clubs; Nine Spades;
                               Ten Hearts; Ten Diamonds; Ten Clubs; Ten Spades;
                               Jack Hearts; Jack Diamonds; Jack Clubs; Jack Spades;
                               Queen Hearts; Queen Diamonds; Queen Clubs; Queen Spades;
                               King Hearts; King Diamonds; King Clubs; King Spades;
                               Ace Hearts; Ace Diamonds; Ace Clubs; Ace Spades;]

let rec shuffle (fulldeck: card array) (acc:deck) : deck = 
  if Array.length acc = 9 then acc else
    let pos = Random.int 52 in 
    shuffle (Array.concat [(Array.sub fulldeck 0 pos);(Array.sub fulldeck (pos+1) (Array.length fulldeck))]) 
      (Array.of_list ((fulldeck.(pos)::(Array.to_list acc)))

       let rec deal (deck:deck) (hand:hand) = 
         if Array.length hand = 2 then hand else
           let pos = Random.int (Array.length deck) in 
           if Array.mem (Array.get deck pos) hand then deal deck hand
           else deal deck (Array.of_list ((Array.get deck pos)::(Array.to_list hand)))
