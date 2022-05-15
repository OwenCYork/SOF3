module Q2vii where
import Pack

{-
### Q2vii [12 marks]
A game of bridge is played either with
* one suit being designated the _trump_ suit, or
* no suit being designated the trump suit.

Give a suitable data type to represent the current trump suit, or that
there is no trump suit. Define the special value `noTrumpSuit::Trumps`
which represents there being no trump suit, and the function
`theTrumpSuit :: Suit -> Trumps`, so that `theTrumpSuit s` is a value
that represents `s` being the trump suit.
-}
noTrumpSuit :: Trumps
theTrumpSuit :: Suit -> Trumps
-- solution ##[6 marks]
type Trumps = Maybe Suit
noTrumpSuit = Nothing
theTrumpSuit = Just

{-
In each round one player leads, and then the other players follow the
lead, in rotation.  The following rules are used to decide the winner
of the round, or _trick_:
* if there is no trump suit, or there is a trump suit but no cards of
  that suit have been exposed, then the highest card of the same suit
  as the led card (the first card exposed) wins.
* if there is a trump suit, and cards of that suit have been exposed,
  then the highest card of the trump suit wins.
* The type `Card` is declared as deriving `Ord`, in a way such that
  the maximum card in a list matches the Bridge notion of highest card
  in a trick.

Implement a function, `trickWinner`, that takes:
* a trump suit, or no trumps,
* a led card, and
* a list of three following cards,

and determines the winning card of the four.

Your solution should satisfy
-}
trickWinnerTest :: Bool
trickWinnerTest =    twh noTrumpSuit             == Card Spades Ace
                  && twh (theTrumpSuit Diamonds) == Card Spades Ace
                  && twh (theTrumpSuit Hearts)   == Card Hearts Four
  where
    twh t = trickWinner t (Card Spades Two) [Card Hearts Four, Card Spades Ace, Card Spades King]

trickWinner :: Trumps -> Card -> [Card] -> Card
-- solution
trickWinner trumps ledCard@(Card ledSuit _) followingCards = maximum possibleWinners
  where
    trick = ledCard : followingCards
    trumpsPlayed = maybe [] (flip selectSuit trick) trumps
    possibleWinners | null trumpsPlayed = selectSuit ledSuit trick
                    | otherwise         = trumpsPlayed
    selectSuit suit hand = [c | c@(Card t _)<-hand, suit==t]

tests :: [(Int, Bool)] 
tests = [(2, trickWinner noTrumpSuit (Card Clubs Ace) [Card Clubs King, Card Hearts Three, Card Clubs Two] == Card Clubs Ace),
         (2, trickWinner (theTrumpSuit Spades) (Card Clubs Ace) [Card Clubs King, Card Hearts Three, Card Clubs Two] == Card Clubs Ace),
         (2, trickWinner (theTrumpSuit Spades) (Card Clubs Ace) [Card Clubs King, Card Spades Three, Card Clubs Two] == Card Spades Three)
        ]
