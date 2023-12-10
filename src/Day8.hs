{-# LANGUAGE OverloadedStrings #-}
module Day8 where

import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as MSet
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

findRoute :: Map String (String, String) -> String -> (String -> Bool) -> String -> Int
findRoute maps directions endTest start = go start directions 0
    where
        go s directions cnt =
            if endTest s
                then cnt
                else go (getNext s (head directions)) (tail directions) (cnt + 1)
        getNext cur dir =
            case next of
                Nothing -> error $ "Unknown key: " <> cur
                Just (left, right) ->
                    if dir == 'L'
                        then left
                        else right
            where
                next = Map.lookup cur maps
            

part1 :: String -> Map String (String, String) -> Int
part1 directions maps =
    findRoute maps (cycle directions) (== "ZZZ") "AAA"


-- Parsing

parsePair :: Parser (String, String)
parsePair = do
    char '('
    left <- many upperChar
    string ", "
    right <- many upperChar
    char ')'
    pure $  (left, right)

parseKey :: Parser String
parseKey = do
    many upperChar

parseMap :: Parser (String, (String, String))
parseMap = do
    key <- many upperChar
    string " = "
    pair <- parsePair
    pure (key, pair)
 
parseData :: Parser (String, Map String (String, String))
parseData = do
    directions <- many upperChar
    many newline
    maps <- sepEndBy1 parseMap eol
    pure (directions, Map.fromList maps)


main :: IO ()
main = do
    (dirs, maps) <- readInput "./data/day8.txt" parseData
    putStrLn $ "Part 1: " <> show (part1 dirs maps)



testData1 :: Text
testData1 = T.pack $ "RL\n\

\AAA = (BBB, CCC)\n\
\BBB = (DDD, EEE)\n\
\CCC = (ZZZ, GGG)\n\
\DDD = (DDD, DDD)\n\
\EEE = (EEE, EEE)\n\
\GGG = (GGG, GGG)\n\
\ZZZ = (ZZZ, ZZZ)\n"

testData2 :: Text
testData2 = T.pack $ "LLR\n\

\AAA = (BBB, BBB)\n\
\BBB = (AAA, ZZZ)\n\
\ZZZ = (ZZZ, ZZZ)\n"