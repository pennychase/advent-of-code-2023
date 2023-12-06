{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day4 where

import qualified Data.Maybe as M
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as MSet
import Data.MultiSet (MultiSet)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

data Card = Card {
    cardId :: Int,
    winners :: Set Int,
    numbers :: Set Int
    } deriving (Show, Eq, Ord)

-- Part 1
numWinners :: Card -> Int
numWinners Card { .. } = Set.size $ Set.intersection winners numbers

score :: Card -> Int
score card =
    let
        n = numWinners card
    in
        if n < 2 then n else 2 ^ (n - 1)

part1 :: [Card] -> Int
part1 = foldr (\x xs -> score x + xs) 0

-- Part 2
part2 :: [Card] -> Int 
       
addCards cardMap cardCounts card@Card { .. }  =
    foldr (\c cs -> MSet.insertMany c count cs) cardCounts newCards
    where
        n = numWinners card
        count = MSet.occur card cardCounts
        newCards = M.catMaybes $ map (\k -> Map.lookup k cardMap) [cardId + 1 .. cardId + n]

part2 cards =
    MSet.size $ foldl (addCards cardMap) cardCounts cards
    where
        cardCounts = MSet.fromList cards
        cardMap = Map.fromList $ zip (map cardId cards) cards     

-- Parsing

parseNumbers :: Parser (Set Int)
parseNumbers = do
    hspace
    numbers <- many intParser
    pure $ Set.fromList numbers

parseCard :: Parser Card
parseCard = do
    string "Card"
    hspace
    num <- intParser
    char ':'
    winners <- parseNumbers
    char '|'
    Card num winners <$> parseNumbers

parseCards :: Parser [Card]
parseCards = do
    sepEndBy1 parseCard eol

main :: IO ()
main = do
    cards <- readInput "./data/day4.txt" parseCards
    putStrLn $ "Part 1: " <> show (part1 cards)
    putStrLn $ "Part 2: " <> show (part2 cards)

-- Testing

testData :: Text
testData = T.pack "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
\Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
\Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
\Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
\Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
\Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"