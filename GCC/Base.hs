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




run :: [GccInstr Int32] -> Int -> EnvChain -> [DataValue] -> Either String (DataValue,EnvChain)
run instrs cp0 env args = run0 $ startstate instrs cp0 env args 

run0 :: State -> Either String (DataValue,EnvChain)
run0 state = case step state of
              Left s       -> Left s
              Right state' -> if stop.codemap $ state' 
                               then case datastack state' of
                                     []    -> Left "Data stack empty at end"
                                     (x:_) -> Right (x,envchain state')
                               else run0 state'
                                     


runGCC :: [GccInstr Int32] -> Worldstate -> Ghostcode -> Either String (DataValue, DataValue -> Worldstate -> Either String (DataValue,Int))
runGCC instrs ws gc  
 = (case run instrs 0 emptyenv [wsToDataValue ws, gcToDataValue gc] of
    Left s -> Left s
    Right (TAG_CONS aistate (TAG_CLOSURE entry k), env) -> Right (aistate, f entry k env)
    Right _ -> Left "wrong result type")
     where f entry k env aistate ws = case run instrs entry (setEnv k env) [aistate,wsToDataValue ws] of
                     Left s -> Left s
                     Right (TAG_CONS aistate' (TAG_INT dir),_) -> Right (aistate', dir)
                     

runDry :: [GccInstr Int32] -> [DataValue] -> Either String DataValue
runDry instrs args = case run instrs 0 emptyenv args of
                       Left s -> Left s
                       Right (value,_) -> Right value

