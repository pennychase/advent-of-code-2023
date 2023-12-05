{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import qualified Data.Maybe as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

data Card = Card Int (Set Int) (Set Int) deriving (Show, Eq, Ord)

data Card' = Card' Int Card deriving (Show, Eq, Ord)

score :: Card -> Int
score (Card _ winners numbers) =
    case n of
        0 -> 0
        1 -> 1
        _ -> 2 ^ (n-1)
    where
        n = S.size $ S.intersection winners numbers

part1 :: [Card] -> Int
part1 = foldr (\x xs -> score x + xs) 0

-- part2 :: [Card] -> Int
part2 cards =
        S.fromList $ map (Card' 0) cards

-- Parsing

parseNumbers :: Parser (Set Int)
parseNumbers = do
    hspace
    numbers <- many intParser
    pure $ S.fromList numbers

parseCard :: Parser Card
parseCard = do
    string "Card"
    hspace
    num <- intParser
    char ':'
    winners <- parseNumbers
    char '|'
    numbers <- parseNumbers
    pure $ Card num winners numbers 

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