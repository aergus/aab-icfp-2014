module Ghc.Test where

import Ghc.Types (GhostWorldView (..))
import Game.Types (GameMap (..), Element (..), GInt)
import Game.Parser (parseGameMap)

import Data.Array
import Data.Ix
import Data.Word

testWorld :: GhostWorldView
testWorld = mapToWorld testMap

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

mapToWorld :: GameMap -> GhostWorldView
mapToWorld (GM mp) = WS {
    lambdaManXY = (snd . head) (mapSearch LambdaManStart mp),
    myIndex = 0,
    ghostStartCoordinates = ghostCoords,
    ghostCurCoordinates   = ghostCoords,
    ghostVitDir           = listArray ghostRange (map (const (0, 2)) ghosts),
    gameMap               = array mapRange
        [(r, fromElement
             ((mp !! (fromIntegral $ snd r)) !! (fromIntegral $ fst r))) |
             r <- range mapRange]
}
    where ghosts      = mapSearch GhostStart mp
          ghostRange  = (0, fromIntegral $ (length ghosts - 1))
          ghostCoords = listArray ghostRange (map snd ghosts)
          width       = fromIntegral $ length (head mp)
          height      = fromIntegral $ length mp
          mapRange    = ((0, 0), (width - 1, height - 1))

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
