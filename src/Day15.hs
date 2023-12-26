{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day15 where

import Data.Char (ord)
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

initMap :: Map Int (Seq (Text, Int))
initMap = M.fromList $ zip [0 .. 255] (repeat Seq.empty)

dispatch :: Map Int (Seq (Text, Int)) -> Text -> Map Int (Seq (Text, Int))
dispatch lenses step
    | T.last step == '-' = removeOp lenses(T.init step)
    | otherwise = addOp lenses step

steps :: [Text] -> Map Int (Seq (Text, Int))
steps xs = go initMap xs
    where
        go m [] = m
        go m (x : xs) = go (dispatch m x) xs

removeOp :: Map Int (Seq (Text, Int)) -> Text -> Map Int (Seq (Text, Int))
removeOp lenses label = 
    case M.lookup (hash label) lenses of
        Nothing -> lenses
        Just box -> rmFromBox label box
    where 
        rmFromBox label box =
            case Seq.findIndexL (\x -> fst x == label) box of
                Nothing -> lenses
                Just i -> M.insert (hash label) (Seq.deleteAt i box) lenses

addOp :: Map Int (Seq (Text, Int)) -> Text -> Map Int (Seq (Text, Int))
addOp lenses step =
    case M.lookup (hash label) lenses of
        Nothing -> lenses
        Just box -> M.insert (hash label) (addToBox (label, fLen) box) lenses 
    where
        [label, n] = T.splitOn "=" step
        fLen = read @Int (T.unpack n)
        addToBox :: (Text, Int) -> Seq (Text, Int) -> Seq (Text, Int)
        addToBox lens box =
            case Seq.findIndexL (\x -> fst x == label) box of
                Nothing -> box |> lens
                Just i -> Seq.update i lens box


focalPower :: Map Int (Seq (Text, Int)) -> Int
focalPower m = sum $ concatMap fpBox boxes
    where
        boxes = filter (\y -> not (Seq.null (snd y))) $ M.assocs m
        fpBox (num, box) = zipWith (*)  [(num + 1) * i | i <- [1 .. (Seq.length box)]]
                                        (map (snd . (Seq.index box)) [0 .. (Seq.length box) - 1])
hash :: Text -> Int
hash str = go 0 str
    where
        go accum s =
            if T.null s
            then accum
            else go ((accum + ord (T.head s)) * 17 `mod` 256) (T.tail s)


part1 :: [Text] -> Int
part1 strs = sum $ map hash strs

part2 :: [Text] -> Int
part2 strs = focalPower $ steps strs

main :: IO ()
main = do
    input <- TIO.readFile "./data/day15.txt"
    let strs = T.splitOn "," (T.stripEnd input)
    putStrLn $ "Part 1: " <> show (part1 strs)
    putStrLn $ "Part 2: " <> show (part2 strs)


-- Testing
testData :: Text
testData = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"   