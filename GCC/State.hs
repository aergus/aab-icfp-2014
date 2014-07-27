module GCC.State where

import Data.Int
import GCC.Types
import GCC.EnvFrame
import qualified Data.IntMap as M

data CodeMap = CodeMap {code :: M.IntMap (GccInstr Int32), stop :: Bool, cp :: Key}



data State = State {datastack    :: DataStack,
                    ctrlstack    :: ControlStack,
                    codemap      :: CodeMap,
                    envchain     :: EnvChain}


halt :: State -> State
halt (state@State {codemap=cm}) = state{codemap=cm{stop=True}}

rapFault :: Int -> Int32 -> State -> Bool
rapFault k l state = k/=(current.envchain$state) || (fromIntegral l)/=(size.currentFrame.envchain$state) || not (dummy.currentFrame.envchain$state)


incCP :: State -> State
incCP (state@State{codemap=cm}) = state {codemap = cm {cp=(cp cm)+1}}

getInstr :: State -> GccInstr Int32
getInstr (state@State{codemap=cm}) = case M.lookup (cp cm) (code cm) of
             Just x  -> x
             Nothing -> error "GCC.State.getInstr"


goto :: Int32 -> State -> State
goto f (state@State{codemap=cm}) = state{codemap = cm{cp=fromIntegral f}}



pushData :: DataValue -> State -> State
pushData val (state@State{datastack=stack})= state{datastack = push val stack}

pushCtrl :: ControlValue -> State -> State
pushCtrl val (state@State{ctrlstack=stack})= state{ctrlstack = push val stack}

 

