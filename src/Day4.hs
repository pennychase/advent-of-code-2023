{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day4 where

import qualified Data.Maybe as M
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MultiSet as MSet
import Data.MultiSet (MultiSet)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

-- Card Id WinningNumbers Numbers
-- data Card = Card Int (Set Int) (Set Int) deriving (Show, Eq, Ord)
data Card = Card {
    id :: Int,
    winners :: Set Int,
    numbers :: Set Int
    } deriving (Show, Eq, Ord)

data CardCount = CardCount {
    count :: Int,
    card :: Card
    } deriving (Show)


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

part2 cards =
    map (addCards cardMap cardCounts) cards
    where
        cardCounts = MSet.fromList cards
        cardMap = Map.fromList cards
       
addCards card@Card { .. } cardMap cardCounts =
    map (MSet.insertMany count cardCounts) newCards
    where
        n = numWinners card
        count = MSet.occurs card cardCounts
        newCards = M.mapMaybe $ M.map (\k -> M.lookup k cardMap) [id + 1 .. id + n]
        


-- part2 :: [Card] -> Int
-- part2 cards =
--         foldl score2 cardMap (Map.elems cardMap)
--     where
--         cardMap = makeMap cards

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
    putStrLn $ "Part 1:" <> show (part1 cards)

-- Testing

testData :: Text
testData = T.pack "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
\Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
\Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
\Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
\Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
\Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"