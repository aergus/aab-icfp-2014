module GCC where
import GCC.Types
import Data.IntMap
import Data.Int

 --TAG_INT, TAG_CONS, TAG_JOIN, TAG_CLOSURE
data DataValue    = TAG_INT Int | TAG_CONS DataValue DataValue | TAG_CLOSURE Int Int
data ControlValue = TAG_JOIN Int| TAG_RET Int| TAG_FRAME Int| TAG_STOP
     
type DataStack        = [DataValue]
type ControlStack     = [ControlValue]
type EnvironmentChain = ([Int],IntMap Environment)
data Environment      = Environment {values :: [DataValue],
                                      esize :: Int,
                                      dummy :: Bool,
                                     parent :: Key}
type Code             = IntMap Instruction
data State = State {datastack    :: DataStack,
                    ctrlstack    :: ControlStack,
                    envchain     :: EnvironmentChain,
                    currentinstr :: Int,
                    currentenv   :: Key,
                          stop   :: Bool}

pop :: [a] -> (a,[a])
pop (x:xs) = (x,xs)
pop []     = error "stack empty"

pop2 :: [a] -> (a,a,[a])
pop2 (x:y:xs) = (x,y,xs)
pop2 _        = error "stack empty"

newEnvironmentChain :: EnvironmentChain
newEnvironmentChain = ([1..],fromList [(0,Environment [] 0 False (error "root has no parent"))])

addEnvironment :: [DataValue] -> Int -> Key -> Bool -> EnvironmentChain -> (EnvironmentChain,Int)
addEnvironment vs n parent dum ((x:xs),intmap) = ((xs,insert x (Environment vs n dum parent) intmap),x)



              
type Instruction = Instr Int

getEnvironment :: EnvironmentChain -> Environment -> Int -> Environment
getEnvironment envs e 0  = e
getEnvironment envs e n  = getEnvironment envs ((snd envs) ! (parent e)) (n-1) 



step :: Code -> State -> State
step code state = let instr = code ! (currentinstr state) in
                   execute instr state


                 
incCP :: State -> State
incCP state = state {currentinstr = (currentinstr state) + 1}

execute :: Instruction -> State -> State
execute (LDC n)  state = incCP $ state {datastack=(TAG_INT n):(datastack state)}
execute (LD n i) state = incCP $ let (envs,stack) = (envchain state, datastack state) in
                                 let e = getEnvironment envs ((snd envs) ! (currentenv state)) n in
                                               state {datastack = ((values e) !! i):stack}
execute ADD      state = incCP $ case pop2 (datastack state) of
                                  (TAG_INT y, TAG_INT x, rest) -> state {datastack = (TAG_INT (x+y)):rest}
                                  _                            -> error "tag mismatch"
execute SUB      state = incCP $ case pop2 (datastack state) of
                                  (TAG_INT y, TAG_INT x, rest) -> state {datastack = (TAG_INT (x-y)):rest}
                                  _                            -> error "tag mismatch"
execute MUL      state = incCP $ case pop2 (datastack state) of
                                  (TAG_INT y, TAG_INT x, rest) -> state {datastack = (TAG_INT (x*y)):rest}
                                  _                            -> error "tag mismatch"
execute DIV      state = incCP $ case pop2 (datastack state) of
                                  (TAG_INT 0, TAG_INT _, _)    -> state
                                  (TAG_INT y, TAG_INT x, rest) -> state {datastack = (TAG_INT (x`div`y)):rest}
                                  _                            -> error "tag mismatch"
execute CEQ      state = incCP $ case pop2 (datastack state) of
                                  (TAG_INT y, TAG_INT x, rest) -> state {datastack = (TAG_INT (if x==y then 1 else 0)):rest}
                                  _                            -> error "tag mismatch"
execute CGT      state = incCP $ case pop2 (datastack state) of
                                  (TAG_INT y, TAG_INT x, rest) -> state {datastack = (TAG_INT (if x>y then 1 else 0)):rest}
                                  _                            -> error "tag mismatch"
execute CGTE     state = incCP $ case pop2 (datastack state) of
                                  (TAG_INT y, TAG_INT x, rest) -> state {datastack = (TAG_INT (if x>=y then 1 else 0)):rest}
                                  _                            -> error "tag mismatch" 
execute ATOM     state = incCP $ case pop (datastack state) of
                                  (TAG_INT _, rest) -> state {datastack = (TAG_INT 1):rest}
                                  (_,rest)          -> state {datastack = (TAG_INT 0):rest}
execute CONS     state = incCP $ let (y,x,rest) = pop2 (datastack state) in
                                  state {datastack = (TAG_CONS x y):rest}
execute CAR      state = incCP $ case pop (datastack state) of
                                  (TAG_CONS x y, rest) -> state {datastack = x:rest}
                                  _                    -> error "tag mismatch"
execute CDR      state = incCP $ case pop (datastack state) of
                                  (TAG_CONS x y, rest) -> state {datastack = y:rest}
                                  _                    -> error "tag mismatch"
execute (SEL n k) state = let newctrlstack = (TAG_JOIN ((currentinstr state)+1)):(ctrlstack state) in
                          case pop (datastack state) of
                           (TAG_INT 0, rest) -> state {datastack=rest, currentinstr=k, ctrlstack=newctrlstack}
                           (TAG_INT _, rest) -> state {datastack=rest, currentinstr=n, ctrlstack=newctrlstack}
execute JOIN     state = case pop (ctrlstack state) of
                           (TAG_JOIN n, rest) -> state {ctrlstack = rest, currentinstr = n}
                           _                  -> error "tag mismatch"
execute (LDF n)  state = incCP $ state {datastack = (TAG_CLOSURE n (currentenv state)):(datastack state)}
execute (AP n)   state = case pop (datastack state) of
                          (TAG_CLOSURE k env, rest)   -> let (val, rest') = splitAt n rest in
                                                         let (newenvs, index)= addEnvironment (reverse val) n env False (envchain state) in
                                                         state {currentenv=index, 
                                                                  envchain=newenvs,
                                                                 datastack=rest',
                                                              currentinstr=k,
                                                                 ctrlstack=(TAG_RET ((currentinstr state)+1)) 
                                                                            :(TAG_FRAME (currentenv state)):(ctrlstack state)}
                          _                           -> error "tag mismatch" 
execute RTN      state = case pop (ctrlstack state) of
                          (TAG_STOP,rest)   -> state {ctrlstack = rest, stop = True}
                          (TAG_RET _,_)     -> case pop2 (ctrlstack state) of
                                                (TAG_RET n, TAG_FRAME k,rest) -> state {currentinstr=n,currentenv=k,ctrlstack=rest}
                                                _              -> error "tag mismatch"   
                          _                 -> error "tag mismatch"
execute (DUM n)  state = incCP $ let (newenvs,index)=addEnvironment [] n (currentenv state) True (envchain state) in
                            state {currentenv=index,envchain = newenvs}
execute (RAP n)  state = case pop (datastack state) of
                          (TAG_CLOSURE k env,rest) -> let cenv = (snd (envchain state)) ! (currentenv state) in
                                                      let (val,rest')=splitAt n rest in
                                                      case(esize cenv == n, dummy cenv, currentenv state == env) of
                                                       (True,True,True)-> state
                                                        {envchain=(fst(envchain state),adjust (\e -> e{values=reverse val, dummy=False}) env (snd(envchain state))),
                                                        ctrlstack=(TAG_RET ((currentinstr state)+1))
                                                                  :(TAG_FRAME (parent cenv)):(ctrlstack state),
                                                     currentinstr=k}
                                                       _          -> error "frame mismatch"  
                          _                        -> error "tag mismatch"
execute STOP     state = state{stop=True}                                                     
                                                        

