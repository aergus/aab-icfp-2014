{- game logic implementation -}

module Game.Main where

import Game.Dummy
import Game.Types

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Maybe
import Data.STRef

runGCC :: GameState s -> LambdaManCode -> ST s (Maybe Integer)
runGCC = undefined

runGHC :: GameState s -> GhostCode -> ST s (Maybe Integer)
runGHC = undefined

lambdaTick :: GameState s -> ST s (GameState s)
lambdaTick gs =
    do utc <- readSTRef (ticks gs)

       lambdaDirs <- mapArray (\ l -> do m <- readSTRef l
                                         dir <- if utc == nextMove (lAgent m)
                                                then runGCC gs (lCode m)
                                                else return Nothing
                                         return dir) (lambdaMen gs)

       ghostDirs <- mapArray (\ g -> do m <- readSTRef g
                                        dir <- if utc == nextMove (gAgent m)
                                               then runGHC gs (gCode m)
                                               else return Nothing
                                        return dir) (ghosts gs)

       -- HACK for going through all array members
       dummy <- mapArray (\ l -> do m <- readSTRef l
                                    dir'' <- readArray lambdaDirs (lIndex m)
                                    dir' <- dir''
                                    when (isJust dir')
                                       (do target <- readArray gm
                                               (dirToCoords (lPos m)
                                                    (fromJust dir'))
                                           when (not $ isWall target)
                                               (modifySTRef l $ moveLambdaMan $
                                                    fromJust dir')))
                    (lambdaMen gs)

       -- TODO: implement prohibited moves for ghosts
       dummy <- mapArray (\ g -> do m <- readSTRef g
                                    dir'' <- readArray ghostDirs (gIndex m)
                                    dir' <- dir''
                                    when (isJust dir')
                                       (do target <- readArray gm
                                               (dirToCoords (gPos m)
                                                    (fromJust dir'))
                                           when (not $ isWall target)
                                               (modifySTRef g $ moveGhost $
                                                    fromJust dir'))) (ghosts gs)

       modifySTRef (frightMode gs) updateFrightMode

       when (utc == 127 * 200 || utc == 127 * 400)
           (writeSTRef (fruitState gs) True)

       when (utc == 127 * 280 || utc == 127 * 480)
           (writeSTRef (fruitState gs) False)

       dummy <- mapArray (\ l -> do m <- readSTRef l
                                    el <- readArray gm (lPos m)
                                    when (isPill el)
                                        (do modifySTRef l (addPoints 10)
                                            modifySTRef (pillCount gs) ((-) 1))
                                    when (isPowerPill el)
                                        (do modifySTRef l (addPoints 50)
                                            dummy' <- mapArray (\ g ->
                                                do modifySTRef g
                                                       (gSetSpeed False)
                                                   return ()) (ghosts gs)
                                            -- TODO: correct fright mode
                                            -- duration, update ghosts
                                            writeSTRef (frightMode gs)
                                                (Just 100))
                                    when (isFruit el)
                                        (do modifySTRef l
                                                (addPoints (fruitPoints $
                                                     level gs)))
                                    when (isEatable el)
                                        (do writeArray gm (lPos m) Empty
                                            modifySTRef l (lSetSpeed False))
                                    return ()) (lambdaMen gs)

       return gs
    where gm = gameMap gs
          lPos = curPos . lAgent
          gPos = curPos . gAgent

moveLambdaMan :: Integer -> LambdaMan -> LambdaMan
moveLambdaMan dir man =
    man { lAgent = moveAgent (lAgent man) dir }

moveGhost :: Integer -> Ghost -> Ghost
moveGhost dir ghost =
   ghost { gAgent = moveAgent (gAgent ghost) dir }

-- TODO: update nextMove
moveAgent :: Agent -> Integer -> Agent
moveAgent agent dir = agent { curPos = dirToCoords pos dir }
   where pos = curPos agent

-- TODO: write this translator
dirToCoords :: (Integer, Integer) -> Integer -> (Integer, Integer)
dirToCoords pos dir = undefined

updateFrightMode :: Maybe Integer -> Maybe Integer
updateFrightMode fright  = if (isNothing fright || fromJust fright == 0)
                           then Nothing
                           else do f <- fright
                                   return (f - 1)

addPoints :: Integer -> LambdaMan -> LambdaMan
addPoints n man = man { lScore = (lScore man) + n }

lSetSpeed :: Bool -> LambdaMan -> LambdaMan
lSetSpeed v man = man { lAgent = setSpeed v (lAgent man) }

gSetSpeed :: Bool -> Ghost -> Ghost
gSetSpeed v ghost = ghost { gAgent = setSpeed v (gAgent ghost) }

setSpeed :: Bool -> Agent -> Agent
setSpeed v agent = agent { fast = v }
