{- game logic implementation -}

module Game.Main where

import Game.Dummy
import Game.Types

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Maybe
import Data.STRef

runGCC :: GameState s -> LambdaManCode -> ST s Integer
runGCC = undefined

runGHC :: GameState s -> GhostCode -> ST s Integer
runGHC = undefined

lambdaTick :: GameState s -> ST s (GameState s)
lambdaTick gs =
    do lambdaDirs <- newArray_ (LOne, LTwo) Nothing
       forM_ (lambdaMen gs) $ \ l -> do dir <- if ticks == nextMove (lAgent l)
                                               then runGCC gs (lCode l)
                                               else Nothing
                                        writeArray lambdaDirs (lIndex l) dir

       ghostDirs <- newArray_ (GOne, GFour) Nothing
       forM_ (ghosts gs) $ \ g -> do dir <- if ticks == nextMove (gAgent g)
                                            then runGHC gs (gCode g)
                                            else Nothing
                                     writeArray ghostDirs (gIndex g) dir

       forM_ (lambdaMen gs) $ \ l -> do dir <- readArray lambdaDirs (lIndex l)
                                        when (isJust dir) $
                                            writeArray
                                                (lambdaMen gs) (lIndex l) $
                                                moveLambdaMan l (gameMap gs) $
                                                    fromJust dir

       forM_ (ghosts gs) $ \ g -> do dir <- readArray ghostDirs (gIndex g)
                                     when (isJust dir) $
                                         writeArray (ghosts gs) (gIndex g) $
                                               moveGhost g (gameMap gs) $
                                                    fromJust dir

       fright <- readSTRef (frightMode 0)
       when (isJust fright) $
           writeSTRef fright (if fromJust fright == 0
                              then Nothing
                              else do { f <- fright; return (f - 1) })

       forM_ (lambdaMen gs) $ \ l -> do pos <- curPos (lAgent l)
                                        element <- readArray (gameMap gs) pos
                                        when (isPowerPill element) $
                                            writeSTRef (frightMode gs)
                                                (Just 127 * 20)

       return gs

moveLambdaMan :: LambdaMan -> GameMap -> Int -> LambdaMan
moveLambdaMan man gameMap dir =
    man { lAgent = moveAgent (lAgent man) gameMap (dirToCoords dir) }

moveGhost :: Ghost -> GameMap -> Int -> Ghost
moveGhost ghost gameMap dir = if odd (dir - curDir agent) &&
                                  readArray gameMap coords == Wall
                              then ghost
                              else ghost { gAgent = moveAgent $
                                  (gAgent ghost) gameMap (dirToCoords dir) }
    where agent  = gAgent ghost
          coords = dirToCoords dir

moveAgent :: Agent -> GameMap -> (Int, Int) -> Agent
moveAgent agent gameMap coords =  if readArray gameMap coords == Wall
                                  then agent
                                  else agent { curPos = coords }
-- TODO: write this translator
dirToCoords :: Int -> (Int, Int)
dirToCoords = undefined
