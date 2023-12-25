{-# LANGUAGE OverloadedStrings #-}

module Day5 (main) where

import qualified Data.List as L
import qualified Data.Maybe as M
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

data Category = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location
    deriving (Show, Eq, Ord)

data IntervalTree = Node ((Int, Int), Int) IntervalTree IntervalTree | Leaf
    deriving (Show, Eq, Ord)

type CategoryMap =  Map Category (Category, IntervalTree)


-- Interval Tree
-- Each node of the tree is a triple consisting of (srcLeft, srcRight, destLeft)

iTreeInsert :: IntervalTree -> (Int, Int, Int) -> IntervalTree
iTreeInsert Leaf (d, s, l) = Node ((s, s + l - 1), d) Leaf Leaf
iTreeInsert (Node n@((lInt, rInt), dest) left right) i@(d, s, l) =
    case compare (s, s + l - 1) (lInt, rInt) of
        LT -> Node n (iTreeInsert left i) right
        EQ -> Node n left right
        GT -> Node n left (iTreeInsert right i)

iTreeFromList :: [(Int, Int, Int)] -> IntervalTree
iTreeFromList intervals = foldl (\ts i -> iTreeInsert ts i) Leaf intervals

iTreeLookup :: IntervalTree -> Int -> Int
iTreeLookup Leaf n = n
iTreeLookup (Node ((l, r), d) left right) n
  | n < l = iTreeLookup left n
  | n >= l && n <= r = d + n - l
  | otherwise = iTreeLookup right n

getDestVals :: CategoryMap -> Category -> [Int] -> Maybe (Category, [Int])
getDestVals cmaps src ns =
    case Map.lookup src cmaps of
        Nothing -> Nothing
        Just (dest, itree) -> Just (dest, map (iTreeLookup itree) ns)

walkMaps :: CategoryMap -> Category -> [Int] -> [Int]
walkMaps cmaps src ns = go src ns
    where
        go src xs =
            case getDestVals cmaps src xs of
                Nothing -> xs
                Just (newSrc, newXs) -> go newSrc newXs

part1 :: CategoryMap -> [Int] -> Int
part1 cmaps seeds = minimum $ walkMaps cmaps Seed seeds

part2 :: CategoryMap -> [Int] -> Int
part2 cmaps seeds = minimum $ walkMaps cmaps Seed seeds'
    where
        seeds' = concat $ makeIntervals seeds
        makeIntervals [] = []
        makeIntervals (start : len : rest) = [start .. start + len - 1] : makeIntervals rest

-- Parsing

parseNumbers :: Parser [Int]
parseNumbers = do
    hspace
    many intParser

parseSeeds :: Parser [Int]
parseSeeds = do
    string "seeds:"
    nums <- parseNumbers
    eol
    pure nums

parseCategory :: Parser Category
parseCategory = do
    Seed <$ string "seed" <|> Soil <$ string "soil" <|> Fertilizer <$ string "fertilizer"
        <|> Water <$ string "water" <|> Light <$ string "light" <|> Temperature <$ string "temperature"
        <|> Humidity <$ string "humidity" <|> Location <$ string "location"

parseInterval :: Parser (Int, Int, Int)
parseInterval = do
    dest <- intParser
    src <- intParser
    len <- intParser
    pure (dest, src, len)

parseMap :: Parser (Category, (Category, IntervalTree))
parseMap = do
    src <- parseCategory
    string "-to-"
    dest <- parseCategory
    string " map:\n"
    intervals <- lineParser parseInterval
    pure (src, (dest, iTreeFromList intervals))

parseMaps :: Parser CategoryMap
parseMaps = do
    maps <-  sepEndBy1 parseMap eol
    pure $ Map.fromList maps

parseData :: Parser ([Int], CategoryMap)
parseData = do
    seeds <- parseSeeds
    eol
    catMap <- parseMaps
    pure (seeds, catMap)


main :: IO ()
main = do
    (seeds, cmaps) <- readInput "./data/day5.txt" parseData
    putStrLn $ "Part 1: " <> show (part1 cmaps seeds)
    putStrLn $ "Part 2: " <> show (part2 cmaps seeds)

-- Testing
testData :: Text
testData = T.pack "seeds: 79 14 55 13\n\
\\n\
\seed-to-soil map:\n\
\50 98 2\n\
\52 50 48\n\
\\n\
\soil-to-fertilizer map:\n\
\0 15 37\n\
\37 52 2\n\
\39 0 15\n\
\\n\
\fertilizer-to-water map:\n\
\49 53 8\n\
\0 11 42\n\
\42 0 7\n\
\57 7 4\n\
\\n\
\water-to-light map:\n\
\88 18 7\n\
\18 25 70\n\
\\n\
\light-to-temperature map:\n\
\45 77 23\n\
\81 45 19\n\
\68 64 13\n\
\\n\
\temperature-to-humidity map:\n\
\0 69 1\n\
\1 0 69\n\
\\n\
\humidity-to-location map:\n\
\60 56 37\n\
\56 93 4\n"