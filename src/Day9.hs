{-# LANGUAGE TypeApplications #-}

module Day9 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable
import Data.Sequence 


makeDifferenceList :: Seq Int -> Seq Int
makeDifferenceList Empty = Empty
makeDifferenceList (_ :<| Empty) = Empty
makeDifferenceList (n1 :<| n2 :<| rest) = n2 - n1 <| makeDifferenceList (n2 :<| rest)

makeDiffLists :: Seq Int -> [Seq Int]
makeDiffLists nums = go [nums] nums
    where
        go accum ns =
            let
                newDiffList = makeDifferenceList ns
            in
                if seqAll(==0) newDiffList
                    then (newDiffList |> 0) : accum
                    else go (newDiffList : accum) newDiffList

seqAll :: (a -> Bool) -> Seq a -> Bool
seqAll pred list =  foldr (\x y -> pred x && y) True list

predict1 :: Seq Int -> Int
predict1 nums = go diffLists
    where
        diffLists = makeDiffLists nums
        go :: [Seq Int] -> Int
        go [_ :|> x] = x
        go (( _ :|> x1) : ( xs :|> x2) : rest) = go ((xs |> (x1 + x2)) : rest)

predict2 :: Seq Int -> Int
predict2 nums = go diffLists
    where
        diffLists = makeDiffLists nums
        go :: [Seq Int] -> Int
        go [x :<| _] = x
        go ((x1 :<| _) : ( x2 :<| xs) : rest) = go (((x2 - x1) <| xs) : rest)

parseInput :: Text -> [Seq Int]
parseInput input =
    map (fromList . lineToInts) (T.lines input)
    where
        lineToInts line = map (read @Int . T.unpack) $ T.words line

part1 :: [Seq Int] -> Int
part1 lists =
    foldr (\x xs -> predict1 x + xs) 0 lists

part2 :: [Seq Int] -> Int
part2 lists =
    foldr (\x xs -> predict2 x + xs) 0 lists
 
main :: IO ()
main = do
    input <- TIO.readFile "./data/day9.txt" 
    let nums = parseInput input
    putStrLn $ "Part 1: " <> show (part1 nums)
    putStrLn $ "Part 2: " <> show (part2 nums)

