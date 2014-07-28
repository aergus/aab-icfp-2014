{-# LANGUAGE DeriveFunctor #-}

module AAB.GCC.Types where
import qualified Data.IntMap as M
import Data.Int

data DataValue    = TAG_INT Int | TAG_CONS DataValue DataValue | TAG_CLOSURE Int Int deriving (Show)
data ControlValue = TAG_JOIN Int| TAG_RET Int| TAG_FRAME Int| TAG_STOP deriving (Show)
     
type DataStack        = [DataValue]
type ControlStack     = [ControlValue]
type Code             = M.IntMap Instruction

pop :: [a] -> Maybe (a,[a])
pop (x:xs) = Just (x,xs)
pop []     = Nothing

pop2 :: [a] -> Maybe (a,a,[a])
pop2 (x:y:xs) = Just (x,y,xs)
pop2 _        = Nothing

poplist :: Int -> [a] -> Maybe([a],[a])
poplist n xs = case length xs < n of
                True -> Nothing
                False -> Just (splitAt n xs)

push :: a -> [a] -> [a]
push = (:)

type Instruction = GccInstr Int32

data GccInstr a =  LDC Int32      --LDC loads int constant 
                 | LD Int32 Int32   --LD n i loads i'th value in n'th frame
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
                 | SEL a a  --sel $t $f pops first elt of stack, if nonzero, go to $t, else to $f. push return adress on stack.
                 | JOIN         --pops first elt, if its an return adress, jump to it.
                 | LDF a      --give literal function address, build closure pointing to that function & with current environment frame
                 | AP Int32       --AP n: pop closure; allocate new environment (child of the current one), fill it with n arguments from the stack. Push stack pointer, environment pointer, return address, then jump into function specified in closure and set env to new frame.
                 | RTN          --pop stack pointer, ret adress, env, restore stack and env, jump to ret adress.
                 | DUM Int32      --creates empty env frame child with specified size, sets it as current.
                 | RAP Int32      --RAP n: pop closure cell. Fill empty frame (pointed to by closure?) with n arguments from the stack. Push stack, parent of current environment, and return address. Set environment frame pointer to frame pointer from closure cell. Jump to code(closure).
                 | STOP deriving (Show, Functor)
                --- TAIL CALL EXT:
