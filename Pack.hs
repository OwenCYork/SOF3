module Pack where
{-
DO NOT MODIFY THIS FILE

Bridge uses a standard fifty-two card pack of cards, with each card
belonging to one of four suits (_Clubs_, _Diamonds_, _Hearts_,
_Spades_).  Each suit has thirteen cards each (labelled from _2_ upto
_10_, and then _Jack_, _Queen_, _King_, _Ace_).

Cards can be described by the data types:
-}
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
           | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Show)

data Card = Card Suit Value deriving (Eq, Ord, Show)
{-
Each player has a _hand_ of cards, represented by the type alias:
-}
type Hand = [Card]

{-
Utility functions for printing cards. (Note: the exact form of the
output depends on the fonts available on your system.)

The function `pp` pretty-prints the result of deal.  It uses the
standard symbols for suits (see `ppsuit`) and values (see `ppval`).
The precise way these symbols appear on your system depends on the
fonts installed thereon, and the access your viewer application has to
the fonts.
-}
pp :: (Hand, Hand, Hand, Hand) -> IO ()
pp = putStrLn . ppdeal

ppdeal :: (Hand, Hand, Hand, Hand) -> String
ppdeal (ns, es, ss, ws) =
  unlines (ppplayer <$> zip ['N', 'E', 'S', 'W'] [ns, es, ss, ws])
  where
    ppplayer (p, cs) = p: ":  " ++ pphand cs

pphand :: Hand -> String
pphand cs = commasep (ppcard <$> cs)
  where
    commasep []              = []
    commasep [xs]            = xs
    commasep (xs:xss@(ys:_)) = xs ++ ", " ++ commasep xss

ppcard :: Card -> String
ppcard (Card s v) = ppsuit s : ppval v
  where
    ppsuit Clubs    = '♣'
    ppsuit Hearts   = '♥'
    ppsuit Diamonds = '♦'
    ppsuit Spades   = '♠'
    --
    ppval Jack  = "J"
    ppval Queen = "Q"
    ppval King  = "K"
    ppval Ace   = "A"
    ppval v     = show ((fromEnum v) + 2)
