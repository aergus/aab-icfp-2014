module GCC.Base where

import GCC.Step
import GCC.State
import GCC.Types
import GCC.EnvFrame
import qualified Data.IntMap as M
import Data.Int

---------------PLACEHOLDER-----------------
type Worldstate = Int                    --
type Ghostcode  = Int                    --
                                         --
wsToDataValue :: Worldstate -> DataValue --
wsToDataValue = TAG_INT                  --
                                         --
gcToDataValue :: Ghostcode -> DataValue  --
gcToDataValue = TAG_INT                  --
-------------------------------------------


emptyenv :: EnvChain
emptyenv = EnvChain{freeKeys = [0..],
                            current=(-1), 
                          envframes=M.fromList []}

startstate :: [GccInstr Int32] -> Int -> EnvChain -> [DataValue] -> State
startstate instrs cp0 env args = State {datastack=[],
                                        ctrlstack=[TAG_STOP],
                                          codemap=CodeMap{stop=False, cp=cp0, code=M.fromList (zip [0..] instrs)},
                                         envchain=newframeWith args env}




run :: Int -> [GccInstr Int32] -> Int -> EnvChain -> [DataValue] -> Either String (DataValue,EnvChain)
run steps instrs cp0 env args = run0 steps $ startstate instrs cp0 env args 

run0 :: Int -> State -> Either String (DataValue,EnvChain)
run0 0 _ = Left "Time's up!"
run0 steps state = case step state of
              Left s       -> Left s
              Right state' -> if stop.codemap $ state' 
                               then case datastack state' of
                                     []    -> Left "Data stack empty at end"
                                     (x:_) -> Right (x,envchain state')
                               else run0 (steps-1) state'
                                     


runGCC :: [GccInstr Int32] -> Worldstate -> Ghostcode -> Either String (DataValue, DataValue -> Worldstate -> Either String (DataValue,Int))
runGCC instrs ws gc  
 = (case run (60000*3072) instrs 0 emptyenv [wsToDataValue ws, gcToDataValue gc] of
    Left s -> Left s
    Right (TAG_CONS aistate (TAG_CLOSURE entry k), env) -> Right (aistate, f entry k env)
    Right _ -> Left "wrong result type")
     where f entry k env aistate ws = case run (1000*3072) instrs entry (setEnv k env) [aistate,wsToDataValue ws] of
                     Left s -> Left s
                     Right (TAG_CONS aistate' (TAG_INT dir),_) -> Right (aistate', dir)
                     

runDry :: [GccInstr Int32] -> [DataValue] -> (Int,Either String DataValue)
runDry instrs args = runDry0 1 $ (startstate instrs 0 emptyenv args)

runDry0 :: Int -> State -> (Int,Either String DataValue)
runDry0 steps state = case step state of
                       Left s -> (steps,Left s)
                       Right state' -> if stop.codemap $ state'
                                  then case datastack state' of
                                        [] -> (steps,Left "Data stack empty at end")
                                        (x:_) -> (steps,Right x)
                                  else runDry0 (steps+1) state'


runDebug :: [GccInstr Int32] -> [DataValue] -> IO ()
runDebug instrs args = runDebug0 0 $ startstate instrs 0 emptyenv args

runDebug0 :: Int -> State -> IO ()
runDebug0 n state = if stop.codemap $ state
                     then do putStrLn ""
                             putStrLn $ "terminated with state " ++ (show n) ++ ":"
                             putStrLn $ show state
                     else do putStrLn ""
                             putStrLn $ "state "++ (show n) ++ ":"
                             putStrLn $ show state
                             case step state of   
                                Left s -> do putStrLn ""
                                             putStrLn $ "ERROR: "++s
                                Right state' -> do getLine
                                                   runDebug0 (n+1) state'

