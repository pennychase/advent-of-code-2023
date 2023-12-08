{-# LANGUAGE TypeApplications #-}

module Day7 where

import Data.Char (isDigit, isLetter)
import Data.List (sortBy, sort, group)
import Data.Ord 

data Hand = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse| FourOfAKind | FiveOfAKind 
    deriving (Show, Eq, Ord)

data Deal = Deal String Int deriving (Show)

getHand :: String -> Hand
getHand str =
    case length cards of
        1 -> FiveOfAKind
        2 -> case head cards of
            4 -> FourOfAKind
            3 -> FullHouse
        3 -> case head cards of
            3 -> ThreeOfAKind
            2 -> TwoPair
        4 -> OnePair
        5 -> HighCard 
    where
        cards = sortBy (comparing Down) $ (map length) . group . sort $ str

compareHands :: String -> String -> Ordering
compareHands hand1' hand2' 
    | hand1 < hand2 = LT
    | hand1 > hand2 = GT
    | otherwise = uncurry compareCards $ head $ dropWhile (\(x,y) -> x ==y) (zip hand1' hand2')
    where
        hand1 = getHand hand1'
        hand2 = getHand hand2'
 
compareCards :: Char -> Char -> Ordering
compareCards card1 card2
    | isDigit card1 && isDigit card2 = compare card1 card2
    | isDigit card1 && isLetter card2 = LT
    | isLetter card1 && isDigit card2 = GT
    | otherwise = compareHighCards card1 card2

compareHighCards :: Char -> Char -> Ordering
compareHighCards card1 card2 =
    case (card1, card2) of
        ('A', _) -> GT
        (_, 'A') -> LT
        ('T', _) -> LT
        (_, 'T') -> GT
        ('K', 'Q') -> GT
        ('Q', 'K') -> LT
        ('K', 'J') -> GT
        ('J', 'K') -> LT
        ('Q', 'J') -> GT
        ('J', 'Q') -> LT
        (_, _) -> EQ

compareDeals :: Deal -> Deal -> Ordering
compareDeals (Deal hand1 _) (Deal hand2 _) = compareHands hand1 hand2

mkDeals :: [String] -> [Deal]
mkDeals strs =
        map mkDeal (map words strs)
    where
        mkDeal [hand, bid] = Deal hand (read @Int bid)

part1 :: [String] -> Int
part1 deals =
       foldr (\(i, (Deal _ bid)) xs -> bid * i + xs) 0 (zip [1 ..] ranked)   
    where
        ranked = sortBy compareDeals $ mkDeals deals

main :: IO ()
main = do
    contents <- readFile "data/day7.txt"
    let input = lines contents
    putStrLn $ "Part 1: " <> show (part1 input)

testData = "32T3K 765\n\
\T55J5 684\n\
\KK677 28\n\
\KTJJT 220\n\
\QQQJA 483\n"