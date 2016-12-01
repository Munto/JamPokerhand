module Pokerhand where
import Data.List

data Suit = Diamond | Clubs
  deriving (Show, Eq, Ord)

data Rank = VIII | IX | X | Jack | Queen | King | Ace
  deriving (Eq, Show, Ord)

data Card = Card Rank Suit
  deriving (Show, Eq, Ord)

rank :: Card -> Rank
rank (Card rank _) = rank

data Hand = Hand Card Card Card Card Card
  deriving (Show)

instance Eq Hand where 
  (==) = undefined 

instance Ord Hand where
  compare = undefined 

sortedcards :: Hand -> [Card]
sortedcards (Hand a b c d e) = (reverse.sort) [a, b, c, d, e]

data Combination = High [Card] 
                 | Pair [Card]
                 | Double [Card]
  deriving (Eq, Show, Ord)

combination hand
  | isDouble hand = Double $ pairCard hand ++ restFrom hand 
  | isPair hand   = Pair $ pairCard hand ++ restFrom hand 
  | otherwise     = High (sortedcards hand) 

isPair :: Hand -> Bool
isPair hand = length(pairsIn hand) == 1

isDouble :: Hand -> Bool
isDouble hand = length(pairsIn hand) == 2

pairCard = concat . pairsIn 

keepOnlyPairs cardGroup = length cardGroup == 2

pairsIn :: Hand -> [[Card]]
pairsIn hand = filter keepOnlyPairs (group cards)
  where cards = sortedcards hand 

restFrom hand =concat $ filter (\l -> length l == 1) 
                               (group (sortedcards hand))




