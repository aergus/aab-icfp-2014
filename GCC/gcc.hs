

 --TAG_INT, TAG_CONS, TAG_JOIN, TAG_CLOSURE
data DataValue    = TAG_INT Int | TAG_CONS Value Value | TAG_JOIN Int | TAG_CLOSURE Int Int
data ControlValue = TAG_JOIN Int| TAG_STOP
     
type DataStack = [DataValue]
type ControlStack = [ControlValue]
type EnvironmentChain = IntMap ([DataValue],Int)
data State = State {datastack :: DataStack,
                    ctrlstack :: ControlStack,
                    envchain  :: EnvironmentChain,
                    currentenv:: Int}

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
              


getEnvironment :: EnvironmentChain -> ([Value],Int) -> Int -> ([Value],Int)
getEnvironment e 0 _ = e
getEnvironment (_,parent) n chain = getEnvironment (chain ! parent) (n-1) chain

execute :: Instruction -> State -> State
execute (LDC n)  state = state {datastack=(TAG_INT n):(datastack state)}
execute (LD n i) state = let (envs,stack) = (envchain state, datastack state) in
                         let (values,_) = getEnvironment envs (envs ! e) n in
                                              state {datastack = (values !! i):stack}
execute ADD      state = let stack = datastack state in
                          state {datastack = f stack}
                           where f [] = error "data stack empty"
                                 f [x] = f []
                                 f ((TAG_INT y):(TAG_INT x):rest) = (TAG_INT x+y):rest
                                 f _                              = error "tag mismatch"
execute SUB      state = let stack = datastack state in
                          state {datastack = f stack}
                           where f [] = error "data stack empty"
                                 f [x] = f []
                                 f ((TAG_INT y):(TAG_INT x):rest) = (TAG_INT x-y):rest
                                 f _                              = error "tag mismatch"
execute MUL      state = let stack = datastack state in
                          state {datastack = f stack}
                           where f [] = error "data stack empty"
                                 f [x] = f []
                                 f ((TAG_INT y):(TAG_INT x):rest) = (TAG_INT x*y):rest
                                 f _                              = error "tag mismatch"
execute DIV      state = let stack = datastack state in
                          state {datastack = f stack}
                           where f [] = error "data stack empty"
                                 f [x] = f []
                                 f xs@((TAG_INT y):(TAG_INT x):rest) = case y of
                                                                     0 -> xs
                                                                     _ -> (TAG_INT x `div` y):rest
                                 f _                              = error "tag mismatch"
execute CEQ      state = let stack = datastack state in
                          state {datastack = f stack}
                           where f [] = error "data stack empty"
                                 f [x] = f []
                                 f ((TAG_INT y):(TAG_INT x):rest) = case x==y of
                                                                        True -> (TAG_INT 1):rest
                                                                        False -> (TAG_INT 0):rest
                                 f _                              = error "tag mismatch" 
execute CGT      state = let stack = datastack state in
                          state {datastack = f stack}
                           where f [] = error "data stack empty"
                                 f [x] = f []
                                 f ((TAG_INT y):(TAG_INT x):rest) = case x>y of
                                                                        True -> (TAG_INT 1):rest
                                                                        False -> (TAG_INT 0):rest
                                 f _                              = error "tag mismatch" 
execute CGTE     state = let stack = datastack state in
                          state {datastack = f stack}
                           where f []                             = error "data stack empty"
                                 f [x]                            = f []
                                 f ((TAG_INT y):(TAG_INT x):rest) = case x>=y of
                                                                        True -> (TAG_INT 1):rest
                                                                        False -> (TAG_INT 0):rest
                                 f _                              = error "tag mismatch" 
execute ATOM     state = let stack = datastack state in
                          state {datastack = f stack}
                           where f []                   = error "data stack empty"
                                 f ((TAG_INT _):xs)     = (TAG_INT 1):xs
                                 f (_:xs)               = (TAG_INT 0):xs
execute CONS     state = let stack = datastack state in
                          state {datastack = f stack}
                           where f []           = error "data stack empty"
                                 f [x]          = f []
                                 f (y:x:rest)   = (TAG_CONS x y):rest
execute CONS     state = let stack = datastack state in
                          state {datastack = f stack}
                           where f []                           = error "data stack empty"
                                 f ((TAG_CONS x _):rest)        = x:rest
                                 f _                            = error "tag mismatch"
                                 

