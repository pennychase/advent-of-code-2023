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

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x,y) = [(x + i, y + j) | j <- [-1,0,1], i <- [-1,0,1]]

isSymbol :: Char -> Bool
isSymbol c 
    | isDigit c = False
    | c == '.' = False
    | otherwise = True

getPartNumber :: Map (Int, Int) Char -> [((Int, Int), Char)] -> Int
getPartNumber grid nums = 
    if any isSymbol $ M.mapMaybe (`Map.lookup` grid) $ S.elems $ S.unions $ map (S.fromList . neighbors . fst) nums
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

makeGrid :: Text -> Map (Int, Int) Char
makeGrid input = Map.fromList $ zip [ (x,y) | x <- dimX, y <- dimY] (T.unpack (T.concat (T.lines input)))
    where
        rows = T.lines input
        dimX = [0 .. length rows - 1]
        dimY = [0 .. T.length (head rows) - 1]

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