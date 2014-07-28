module Game.Test where

import Game.Types (GameMap (..), Element (..), GInt)
import Game.Parser (parseGameMap)

import Data.Ix
import Data.Word

testMap :: GameMap
testMap = either (const (GM [[]])) id $ parseGameMap (
    "#######################\n" ++
    "#..........#..........#\n" ++
    "#.###.####.#.####.###.#\n" ++
    "#o###.####.#.####.###o#\n" ++
    "#.....................#\n" ++
    "#.###.#.#######.#.###.#\n" ++
    "#.....#....#....#.....#\n" ++
    "#####.#### # ####.#####\n" ++
    "#   #.#    =    #.#   #\n" ++
    "#####.# ### ### #.#####\n" ++
    "#    .  # === #  .    #\n" ++
    "#####.# ####### #.#####\n" ++
    "#   #.#    %    #.#   #\n" ++
    "#####.# ####### #.#####\n" ++
    "#..........#..........#\n" ++
    "#.###.####.#.####.###.#\n" ++
    "#o..#......\\......#..o#\n" ++
    "###.#.#.#######.#.#.###\n" ++
    "#.....#....#....#.....#\n" ++
    "#.########.#.########.#\n" ++
    "#.....................#\n" ++
    "#######################\n")

fromElement :: Element -> GInt
fromElement Wall           = 0
fromElement Empty          = 1
fromElement Pill           = 2
fromElement PowerPill      = 3
fromElement Fruit          = 4
fromElement LambdaManStart = 5
fromElement GhostStart     = 6

mapSearch :: (Eq a, Num b) => a -> [[a]] -> [(a, (b, b))]
mapSearch t ts = filter ((== t) . fst)
    [((ts !! (snd r)) !! (fst r),
         (fromIntegral $ fst r, fromIntegral $ snd r)) |
         r <- range ((0, 0), (length (head ts) - 1, length ts - 1))]
