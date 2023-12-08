{-# LANGUAGE TypeApplications #-}

module Day7 where

import Data.Char (isDigit, isLetter)
import Data.List (sortBy, sort, group, nub)
import Data.Ord 

data Hand = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse| FourOfAKind | FiveOfAKind 
    deriving (Show, Eq, Ord)

data Card = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | T | J | Q | K | A
    deriving (Show, Eq, Ord)

data Card2 = J' | C1' | C2' | C3' | C4' | C5' | C6' | C7' | C8' | C9' | T' | Q' | K' | A'
    deriving (Show, Eq, Ord)


charToCard1 :: Char -> Card
charToCard1 c =
    case c of
        'A' -> A
        'K' -> K
        'Q' -> Q
        'J' -> J
        'T' -> T
        '9' -> C9
        '8' -> C8
        '7' -> C7
        '6' -> C6
        '5' -> C5
        '4' -> C4
        '3' -> C3
        '2' -> C2
        '1' -> C1

cardToCard2 :: Card -> Card2
cardToCard2 c =
    case c of
        A -> A'
        K -> K'
        Q -> Q'
        J -> J'
        T -> T'
        C9 -> C9'
        C8 -> C8'
        C7 -> C7'
        C6 -> C6'
        C5 -> C5'
        C4 -> C4'
        C3 -> C3'
        C2 -> C2'
        C1 -> C1'


data Deal = Deal [Card] Int deriving (Show)

getHand :: [Card] -> Hand
getHand cards =
    case length cardsSorted of
        1 -> FiveOfAKind
        2 -> case head cardsSorted of
            4 -> FourOfAKind
            3 -> FullHouse
        3 -> case head cardsSorted of
            3 -> ThreeOfAKind
            2 -> TwoPair
        4 -> OnePair
        5 -> HighCard 
    where
        cardsSorted = sortBy (comparing Down) $ (map length) . group . sort $ cards

compareHands1 :: [Card] -> [Card] -> Ordering
compareHands1 hand1 hand2
    | hand1' < hand2' = LT
    | hand1' > hand2' = GT
    | otherwise = uncurry compare  $ head $ dropWhile (\(x,y) -> x ==y) (zip hand1 hand2)
    where
        hand1' = getHand hand1
        hand2' = getHand hand2

compareHands2 :: [Card] -> [Card] -> Ordering
compareHands2 hand1 hand2
    | hand1' < hand2' = LT
    | hand1' > hand2' = GT
    | otherwise = uncurry compare  $ head $ dropWhile (\(x,y) -> x ==y) (zip hand1'' hand2'')
    where
        hand1' = getPossibleHand hand1
        hand2' = getPossibleHand hand2
        hand1'' = map cardToCard2 hand1
        hand2'' = map cardToCard2 hand2

getPossibleHand :: [Card] -> Hand
getPossibleHand cards =
    case n of 
        5 -> getHand cards   -- no jokers, so find the hand of the cards
        0 -> FiveOfAKind     -- all Jokers, so the hand is five of a kind
        _ -> maximum (map getHand possibleHands)   -- some jokers, so find the highest possible hand
    where
        others = filter (/= J) cards
        n = length others
        numJs = 5 - n
        possibleCards = nub $ (map sort) $ sequence (replicate numJs (nub others))
        possibleHands = map (<> others) possibleCards
 
compareDeals :: ([Card] -> [Card] -> Ordering) -> Deal -> Deal -> Ordering
compareDeals cmp (Deal hand1 _) (Deal hand2 _) = cmp hand1 hand2

mkDeals :: [String] -> [Deal]
mkDeals strs =
        map mkDeal (map words strs)
    where
        mkDeal [hand, bid] = Deal (map charToCard1 hand) (read @Int bid)

part1 :: [String] -> Int
part1 deals =
       foldr (\(i, (Deal _ bid)) xs -> bid * i + xs) 0 (zip [1 ..] ranked)   
    where
        ranked = sortBy (compareDeals compareHands1) $ mkDeals deals

part2 :: [String] -> Int
part2 deals =
       foldr (\(i, (Deal _ bid)) xs -> bid * i + xs) 0 (zip [1 ..] ranked)   
    where
        ranked = sortBy (compareDeals compareHands2) $ mkDeals deals

main :: IO ()
main = do
    contents <- readFile "data/day7.txt"
    let input = lines contents
    putStrLn $ "Part 1: " <> show (part1 input)
    putStrLn $ "Part 2: " <> show (part2 input)


testData = "32T3K 765\n\
\T55J5 684\n\
\KK677 28\n\
\KTJJT 220\n\
\QQQJA 483\n"
