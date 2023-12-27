module Day3 where

import Data.Char (isDigit)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Maybe as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Debug.Trace 

readInt :: String -> Int
readInt str = read str 

neighbors :: [(Int, Int)] -> S.Set (Int, Int)
neighbors nums = foldr S.delete (S.unions $ map f nums) nums
    where
        f (x, y) = S.fromList [(x + i, y + j) | j <- [-1,0,1], i <- [-1,0,1]]

isSymbol :: Char -> Bool
isSymbol c 
    | isDigit c = False
    | c == '.' = False
    | otherwise = True

isStar :: Char -> Bool
isStar c
    | c == '*' = True
    | otherwise = False

getPartNumber :: Map (Int, Int) Char -> [((Int, Int), Char)] -> Int
getPartNumber grid nums =  
    if any isSymbol $ M.mapMaybe (`Map.lookup` grid) $ S.elems $ neighbors (map fst nums)
        then readInt (map snd nums)
        else 0

getParts :: Map (Int, Int) Char -> Int
getParts grid = go 0 (Map.assocs grid)
    where
        go :: Int -> [((Int, Int), Char)] -> Int
        go accum xs =
            case L.findIndex (isDigit . snd) xs of
                Nothing -> accum
                Just n ->
                    let 
                        (ys, ys') = L.span (isDigit . snd) $ drop n xs
                    in go (accum + getPartNumber grid ys) ys' 

getGearRatios :: Map (Int, Int) Char -> Int
getGearRatios grid = sum $ map (getGearRatio grid) (Map.keys (Map.filter (== '*') grid))

getGearRatio :: Map (Int, Int) Char -> (Int, Int) -> Int
getGearRatio grid star = 
    if length gears < 2 then 0 else product gears
    where
        adjacent = Map.keys $ Map.filterWithKey (\k a -> k `elem` S.elems (neighbors [star]) && isDigit a) grid
        gears = S.elems $ S.fromList $ map (findNum grid . findNumStart grid) adjacent


findNumStart :: Map (Int, Int) Char -> (Int, Int) -> (Int, Int)
findNumStart grid (x, y) = go (x, y) (x, y)
    where
        go accum (r, c) =
            case Map.lookup (r,c) grid of
                Nothing -> accum
                Just val -> 
                    if isDigit val
                        then go (r,c) (r, c - 1) 
                        else accum

findNum :: Map (Int, Int) Char -> (Int, Int) -> Int
findNum grid (x, y) = readInt . reverse $ go "" (x, y)
    where
        go accum (r, c) =
            case Map.lookup (r,c) grid of
                Nothing -> accum
                Just val -> 
                    if isDigit val
                        then go (val : accum) (r, c + 1) 
                        else accum


makeGrid :: Text -> Map (Int, Int) Char
makeGrid input = Map.fromList $ zip [ (x,y) | x <- dimX, y <- dimY] (T.unpack (T.concat rows'))
    where
        rows = T.lines input
        rows' = map (`T.snoc` '.') rows
        dimX = [0 .. length rows' - 1]
        dimY = [0 .. T.length (head rows') - 1]

toRows :: Map (Int, Int) Char -> [[((Int, Int), Char)]]
toRows grid = L.groupBy (\((r1,_), _) ((r2,_),_) -> r1 ==r2)$ Map.assocs grid

showGrid :: Map (Int, Int) Char -> IO ()
showGrid grid = do
    mapM_ (putStrLn . map snd) (toRows grid)


main :: IO ()
main = do
    contents <- TIO.readFile "./data/day3.txt"
    let grid = makeGrid contents
    putStrLn $ "Part 1: " <> show (getParts grid)
    putStrLn $ "Part 2: " <> show (getGearRatios grid)

testData :: Text
testData = T.pack "467..114..\n\
\...*......\n\
\..35..633.\n\
\......#...\n\
\617*......\n\
\.....+.58.\n\
\..592.....\n\
\......755.\n\
\...$.*....\n\
\.664.598..\n"

testData2 :: Text 
testData2 = T.pack "12.......*..\n\
\+.........34\n\
\.......-12..\n\
\..78........\n\
\..*....60...\n\
\78..........\n\
\.......23...\n\
\....90*12...\n\
\............\n\
\2.2......12.\n\
\.*.........*\n\
\1.1.......56\n"