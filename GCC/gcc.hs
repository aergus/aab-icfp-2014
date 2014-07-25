

 --TAG_INT, TAG_CONS, TAG_JOIN, TAG_CLOSURE
data DataValue    = TAG_INT Int | TAG_CONS Value Value | TAG_JOIN Int | TAG_CLOSURE Int Int
data ControlValue = TAG_JOIN Int| TAG_RET Int| TAG_FRAME Int| TAG_STOP
     
type DataStack        = [DataValue]
type ControlStack     = [ControlValue]
type EnvironmentChain = ([Int],IntMap Environment)
data Environment      = {values :: [DataValue]
                           size :: Int
                          dummy :: Bool
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

addEnvironment :: [DataValue] -> Int -> Key -> EnvironmentChain -> (EnvironmentChain,Int)
addEnvironment vs n parent ((x:xs),intmap) = ((xs,insert x (Environment vs n False parent) intmap),x)

updateEnvironment

data Instruction = LDC Int      --LDC loads int constant
                 | LD Int Int   --LD n i loads i'th value in n'th frame
                 | ADD          --int add
                 | SUB          --int sub
                 | MUL          --int mult
                 | DIV          --int div
                 | CEQ          --equal
                 | CGT          --greater than
                 | CGTE         --greater than or equal
                 | ATOM         --checks whether top elt of the stack is int, consumes it
                 | CONS         --builds cons cell of top two elements of stack, pushes it
                 | CAR          --extracts first element of cons cell
                 | CDR          --extracts second element of cons cell
                 | SEL          --sel $t $f pops first elt of stack, if nonzero, go to $t, else to $f. push return adress on stack.
                 | JOIN         --pops first elt, if its an return adress, jump to it.
                 | LDF Int      --give literal function address, build closure pointing to that function & with current environment frame
                 | AP Int       --AP n: pop closure; allocate new environment (child of the current one), fill it with n arguments from the stack. Push stack pointer, environment pointer, return address, then jump into function specified in closure and set env to new frame.
                 | RTN          --pop stack pointer, ret adress, env, restore stack and env, jump to ret adress.
                 | DUM Int      --creates empty env frame child with specified size, sets it as current.
                 | RAP Int      --RAP n: pop closure cell. Fill empty frame (pointed to by closure?) with n arguments from the stack. Push stack, parent of current environment, and return address. Set environment frame pointer to frame pointer from closure cell. Jump to code(closure).
                 | STOP
                --- TAIL CALL EXT:
              


getEnvironment :: EnvironmentChain -> Environment -> Int -> Environment
getEnvironment envs e 0 _ = e
getEnvironment envs e n  = getEnvironment ((snd envs) ! (parent e)) (n-1) chain



step :: Code -> State -> State
step code state = let instr = code ! (currentinstr state) in
                   execute instr state
                  
incCP :: State -> State
incCP state = state {currentinstr = (currentinstr state) + 1}

execute :: Instruction -> State -> State
execute (LDC n)  state = incCP $ state {datastack=(TAG_INT n):(datastack state)}
execute (LD n i) state = incCP $ let (envs,stack) = (envchain state, datastack state) in
                                 let e = getEnvironment envs (envs ! (currentenv state)) n in
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
                                  state {datastack = (TAG_CONS x y):rest
execute CAR      state = incCP $ case pop (datastack state) of
                                  (TAG_CONS x y, rest) -> state {datastack = x:rest}
                                  _                    -> error "tag mismatch"
execute CDR      state = incCP $ case pop (datastack state) of
                                  (TAG_CONS x y, rest) -> state {datastack = y:rest}
                                  _                    -> error "tag mismatch"
execute (SEL n k) state = let newctrlstack = (TAG_JOIN ((currentinstr state)+1)):(ctrlstack state)
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
                                                          environmentchain=newenvs,
                                                                 datastack=rest'
                                                              currentinstr=k
                                                              controlstack=(TAG_RET ((currentinstr state)+1)) 
                                                                            :(TAG_FRAME (currentenv state)):(controlstack state)
                          _                           -> error "tag mismatch" 
execute RTN      state = case pop (ctrlstack state) of
                          (TAG_STOP,rest)   -> state {ctrlstack = rest, stop = True}
                          (TAG_RET _,_)     -> case pop2 (ctrlstack state) of
                                                (TAG_RET n, TAG_FRAME k,rest) -> state {currentinstr=n,currentenv=k,ctrlstack=rest}
                                                _              -> error "tag mismatch"   
                          _                 -> error "tag mismatch"
execute (DUM n)  state = incCP $ let (newenvs,index)=addEnvironment [] n (currentenv state) True (envchain state) in
                            state {currentenv=index}
execute (RAP n)  state = case pop (datastack state) of
                          (TAG_CLOSURE k env,rest) -> let cenv = (envchain state) ! (currentenv state) in
                                                      let (val,rest')=splitAt n rest in
                                                      case(islengthn,isdummy) = (size cenv == n, dummy cenv) of
                                                       (True,True)-> state
           {-THIS MIGHT BE WRONG-}                      {envchain=(adjust (\e -> e{values=reverse val}) (currentenv state)).(adjust (\e->e{dummy=False}) env)$(envchain state),
   
                                                        ctrlstack=(TAG_RET ((currentinstr state)+1))
                                                                  :(TAG_FRAME (parent cenv)):(ctrlstack state)
                                                     currentinstr=k
                                                       currentenv=env}
                                                       _          -> error "frame mismatch"  
                          _                        -> error "tag mismatch"
execute STOP     state = state{stop=True}                                                     
                                                        

