{-# LANGUAGE TypeApplications #-}

module Day6 where


data Race = Race Int Int deriving (Show)

race :: Race -> Int
race (Race time distance) =
    length $ filter (> distance) $ map (\(x, y) -> x * y) (zip [0 .. time] [time, time - 1 .. 0])

-- Input is a list of two strings: ["Time:    7 15   30","Distance: 9  40  200"\
part1 :: [String] -> Int
part1 input = product $ map race races
    where
        times = map (read @Int) $ (tail . words . head) input
        distances = map (read @Int) $ (tail . words . last) input
        races = zipWith Race times distances

part2 :: [String] -> Int
part2 input = race (Race time distance)
    where
        time = read @Int $ (concat . tail . words . head) input
        distance = read @Int $ (concat . tail . words . last) input

main :: IO () 
main = do
    contents <- readFile "./data/day6.txt"
    let input = lines contents
    putStrLn $ "Part 1: " <> show (part1 input)
    putStrLn $ "Part 2: " <> show (part2 input)

-- Data
testData = "Time:      7  15   30\nDistance:  9  40  200\n"


