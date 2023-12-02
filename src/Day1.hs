{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.Char (isDigit, digitToInt)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


part1 :: [T.Text] -> Int
part1 = foldr (\l ls -> compute l + ls) 0
    where
        compute xs = 
            let
                ns = T.filter isDigit xs
            in
                if T.null $ T.filter isDigit ns
                    then 0
                    else 10 * digitToInt (T.head ns) + digitToInt (T.last ns)

part2 :: [T.Text] -> Int
part2 = foldr (\l ls -> compute l + ls) 0
    where
        compute xs = 
            let ns = getNums xs
            in case ns of
                [] -> 0
                _ -> 10 * head ns + last ns

        getNums :: T.Text -> [Int]
        getNums str 
            | T.null str = []
            | otherwise = f str <> getNums (T.tail str)
            where
                f str
                    | T.null str = []
                    | n `T.elem` "0123456789" = [digitToInt n]
                    | T.isPrefixOf "one" str = [1]
                    | T.isPrefixOf "two" str = [2]
                    | T.isPrefixOf "three" str = [3]
                    | T.isPrefixOf "four" str = [4]
                    | T.isPrefixOf "five" str = [5]
                    | T.isPrefixOf "six" str = [6]
                    | T.isPrefixOf "seven" str = [7]
                    | T.isPrefixOf "eight" str = [8]
                    | T.isPrefixOf "nine" str = [9]
                    | otherwise = []
                    where
                        n = T.head str
                    

main :: IO ()
main = do
    contents <- TIO.readFile "./data/day1.txt"
    let lines = T.lines contents
    print $ part1 lines
    print $ part2 lines

-- Part 1 test data
testData1 :: [T.Text]
testData1 =
    [ "1abc2"
    , "pqr3stu8vwx"
    , "a1b2c3d4e5f"
    , "treb7uchet"
    ]

-- Part 2 test data
testData2 :: [T.Text]
testData2 = 
    [ "two1nine"
    , "eightwothree"
    , "abcone2threexyz"
    , "xtwone3four"
    , "4nineeightseven2"
    , "zoneight234"
    , "7pqrstsixteen"
    ]