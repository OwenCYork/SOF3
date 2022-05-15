module Q2i where
import Pack
{-
### Q2i [2 marks]
Define a _pack_ of cards as a list of `Card`, containing each card
exactly once.  The suits must be arranged in the order: _Clubs_,
_Diamonds_, _Hearts_, _Spades_; within each suit the cards must be
arranged in the order _Two_, _Three_, ..., _Ten_, _Jack_, _Queen_,
_King_, _Ace_.

Types `Card`, `Suit` and `Value` are defined in the imported module
`Pack`, which you are recommended to read (but must **not** modify)
as well as utility functions for printing structures containing cards.

Your solution should satisfy:
-}
packTest :: Bool
packTest =
  pack!!3 == Card Clubs Five
  && pack!!4 == Card Clubs Six
  && pack!!16 == Card Diamonds Five
  && pack!!31 == Card Hearts Seven
  && pack!!48 == Card Spades Jack
{-
To gain full marks your solution should take advantage of the fact
that the types used to describe `Card` are instances of `Enum`.
-}
pack :: [Card]

pack = [Card s v | s <- [Clubs .. Spades], v <- [Two .. Ace]]


tests :: [(Int, Bool)] 
tests = zipWith f [5,51] [Card Clubs Seven,Card Spades Ace]
  where f i c = (1, pack!!i == c)
