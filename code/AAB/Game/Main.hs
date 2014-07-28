{- game logic implementation (uses old types, hence not even correctly typed) -}

module AAB.Game.Main where

import AAB.Game.Dummy
import AAB.Game.Types

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

-- TODO: handle/correct finishing condition details
lambdaLoop :: GameState s -> ST s (LambdaMan, Integer, Integer)
lambdaLoop gs =
  do gs' <- lambdaTick gs
     pills <- readSTRef (pillCount gs')
     m <- readArray (lambdaMen gs') LOne
     man <- readSTRef m
     utc <- readSTRef (ticks gs')
     if (pills == 0 ||
             lLives man == 0 ||
             utc > 127 * (fst $ mapDims gs') * (snd $ mapDims gs') * 16)
     then return (man, utc, pills)
     else do { modifySTRef (ticks gs') (+1); lambdaLoop gs' }

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

       dummy <- mapArray (\ g -> do m <- readSTRef g
                                    dir'' <- readArray ghostDirs (gIndex m)
                                    dir' <- dir''
                                    when (isJust dir')
                                       (do target <- readArray gm
                                               (dirToCoords (gPos m)
                                                    (fromJust dir'))
                                           when (not $ isWall target)
                                               (if (even (curDir (gAgent m) -
                                                        fromJust dir'))
                                                then modifySTRef g $ moveGhost $
                                                        fromJust dir'
                                                -- TODO: do the right thing here
                                                else modifySTRef g $ moveGhost $
                                                        fromJust dir')))
                                    (ghosts gs)

       fright <- readSTRef (frightMode gs)
       when (isJust fright) $
           if (fromJust fright) == 1
           then do dummy <- mapArray (\ g -> do modifySTRef g (setFright False))
                       (ghosts gs)
                   writeSTRef (frightMode gs) Nothing
           else do writeSTRef (frightMode gs)
                       (do { f <- fright; return (f - 1) })

       when (utc == 127 * 200 || utc == 127 * 400)
           (writeSTRef (fruitState gs) True)

       when (utc == 127 * 280 || utc == 127 * 480)
           (writeSTRef (fruitState gs) False)

       fruitOn <- readSTRef (fruitState gs)

       dummy <- mapArray (\ l -> do m <- readSTRef l
                                    el <- readArray gm (lPos m)
                                    when (isPill el)
                                        (do modifySTRef l (eatPoints 10)
                                            modifySTRef (pillCount gs) ((-) 1))
                                    when (isPowerPill el)
                                        (do modifySTRef l (eatPoints 50)
                                            dummy' <- mapArray (\ g ->
                                                do modifySTRef g
                                                       (setFright True))
                                                (ghosts gs)
                                            -- TODO: fright mode duration
                                            writeSTRef (frightMode gs)
                                                (Just 1000))
                                    when (isFruit el && fruitOn)
                                        (do modifySTRef l
                                                (eatPoints (fruitPoints $
                                                     level gs)))
                                    when (isEatable el)
                                        (do writeArray gm (lPos m) Empty)
                                    dummy' <- mapArray (\ g ->
                                        do m' <- readSTRef g
                                           when (lPos m == gPos m' &&
                                                     gVisible m')
                                               (do f <- readSTRef
                                                       (frightMode gs)
                                                   if isNothing f
                                                   then do dummy'' <- mapArray
                                                               (\ g' ->
                                                                modifySTRef g'
                                                                (reset True))
                                                               (ghosts gs)
                                                           modifySTRef l die
                                                   -- TODO: eat ghost
                                                   else do modifySTRef g
                                                               (reset False)))
                                        (ghosts gs)
                                    return ()) (lambdaMen gs)

       dummy <- mapArray (\ l -> do m <- readSTRef l
                                    dir'' <- readArray lambdaDirs (lIndex m)
                                    dir' <- dir''
                                    when (isJust dir')
                                       (do modifySTRef l (lUpdateNav $
                                               fromJust dir')))
           (lambdaMen gs)

       dummy <- mapArray (\ g -> do m <- readSTRef g
                                    dir'' <- readArray ghostDirs (gIndex m)
                                    dir' <- dir''
                                    when (isJust dir')
                                       (do modifySTRef g (gUpdateNav $
                                               fromJust dir')))
           (ghosts gs)

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

dirToCoords :: (Integer, Integer) -> Integer -> (Integer, Integer)
dirToCoords (x, y) 0 = (x, y - 1)
dirToCoords (x, y) 1 = (x + 1, y)
dirToCoords (x, y) 2 = (x, y + 1)
dirToCoords (x, y) 3 = (x - 1, y)
dirToCoords _      _ = undefined -- TODO: add exception handling?

eatPoints :: Integer -> LambdaMan -> LambdaMan
eatPoints n man =
    man { lAgent = setSpeed False (lAgent man), lScore = (lScore man) + n }

setFright :: Bool -> Ghost -> Ghost
setFright v ghost = ghost { gAgent = newAgent, gFrightened = v }
   where a = setSpeed (not v) (gAgent ghost)
         d = curDir a
         newAgent = a { curDir = if v then mod (d + 2) 4 else d }

lSetSpeed :: Bool -> LambdaMan -> LambdaMan
lSetSpeed v man = man { lAgent = setSpeed v (lAgent man) }

gSetSpeed :: Bool -> Ghost -> Ghost
gSetSpeed v ghost = ghost { gAgent = setSpeed v (gAgent ghost) }

setSpeed :: Bool -> Agent -> Agent
setSpeed v agent = agent { fast = v }

die :: LambdaMan -> LambdaMan
die man = man { lAgent = backToStart (lAgent man), lLives = lLives man - 1 }

reset :: Bool -> Ghost -> Ghost
reset v ghost = ghost { gAgent = backToStart (gAgent ghost), gVisible = v }

backToStart :: Agent -> Agent
backToStart agent = agent { curPos = initPos agent, curDir = initDir agent }

lUpdateNav :: Integer -> LambdaMan -> LambdaMan
lUpdateNav n man = man { lAgent = updateNav n (lAgent man) }

gUpdateNav :: Integer -> Ghost -> Ghost
gUpdateNav n ghost = ghost { gAgent = updateNav n (gAgent ghost) }

updateNav :: Integer -> Agent -> Agent
updateNav n agent = agent { curDir = n, nextMove = nm }
    where m  = nextMove agent
          nm = m + if fast agent then primTPM agent else secTPM agent
