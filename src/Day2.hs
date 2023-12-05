{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

data Game = Game Int [[(Int, Color)]] deriving Show

-- Part 1
-- Given number of each color, which games are possible

-- Game is possible if the number of each color is less than the given numbers
validCount :: (Int, Color) -> Bool
validCount (cnt, color) =
    case color of
        Red -> cnt <= 12
        Green -> cnt <= 13
        Blue -> cnt <= 14 

-- Find all possible games and sum their game numbers
possible :: [Game] -> Int
possible = foldr (\g gs -> validGame g + gs) 0
    where
        validGame (Game n counts) = 
            if all validCount (concat counts)
                then n
                else 0

-- Part 2
-- Given a set of games, what is the minimum number of each color that is possible

-- Find the minimum number for each color (i.e., the maximum number across all the "reveals")
-- and multiple these to obtain the power of the game
powerOfMinValues :: Game -> Int
powerOfMinValues (Game _ cnts) =
    foldr (\y ys ->  (maximum . map fst) y * ys) 1 xs
    where
        -- group the counts by Color
        xs = L.groupBy (\(_,c1) (_,c2) -> c1 == c2) $ L.sortBy (\(_,c1) (_,c2) -> compare c1 c2) $ concat cnts

-- sum the powers for all the games
sumOfPowers :: [Game] -> Int
sumOfPowers = foldr (\g gs -> powerOfMinValues g + gs) 0

-- Parsing


parseCount :: Parser (Int, Color)
parseCount = do
    hspace
    cnt <- intParser
    hspace
    color <- Red <$ string "red" <|> Green <$ string "green" <|> Blue <$ string "blue"
    pure (cnt, color)


parseGame :: Parser Game
parseGame = do
    string "Game "
    game <- intParser
    char ':'
    Game game <$> (csvParser parseCount) `sepBy` (char ';')

parseGames :: Parser [Game]
parseGames = do
    sepEndBy1 parseGame eol

main :: IO ()
main = do
    games <- readInput "./data/day2.txt" parseGames
    putStrLn $ "Part 1: " <> show (possible games)
    putStrLn $ "Part 2: " <> show (sumOfPowers games)

testData = concat
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n"
    , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n"
    , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n"
    , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n"
    , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"
    ]