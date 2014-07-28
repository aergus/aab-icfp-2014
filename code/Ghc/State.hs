module Ghc.State where

import Ghc.Types
import Data.Word
import Data.Bits
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Array.MArray
import Data.Array.Unboxed

step :: GhostWorldView -> GhcState s -> ST s ()
step world state = do
        term <- readSTRef (terminate state)
        unless term $ do
                loadInstruction world state
                c <- readSTRef (counter state)
                when (c >= 1024) $ writeSTRef (terminate state) True

loadInstruction :: GhostWorldView -> GhcState s -> ST s ()
loadInstruction world state = do
	pc <- readArray (registers state) PC
	doInstruction world state (code state ! pc)
	newpc <- readArray (registers state) PC
	when (newpc == pc) $ writeArray (registers state) PC (pc + 1)
        modifySTRef (counter state) succ

interrupt :: GhostWorldView -> GhcState s -> Word8 -> ST s ()
--
--
--    INT 0
--        In:
--            Register A: ghost’s new direction
--
--    Set the direction of the ghost. 0 is up; 1 is right; 2 is down; 3 is left.
--
--    The direction of the ghost is set at the end of the game cycle. If the interrupt is called multiple times in a single game cycle, the last interrupt overrides any earlier ones. Using an invalid direction in register A is equivalent to retaining the ghost’s original direction at the beginning of the game cycle.
--
interrupt world state 0 = do
        newdir <- readArray (registers state) A
        writeSTRef (dir state) newdir
--    INT 1
--        Out:
--            Register A: First Lambda-Man’s x-ordinate
--            Register B: First Lambda-Man’s y-ordinate
--
--    Stores the first Lambda-Man’s position in registers A (x-ordinate) and B (y-ordinate). In the single Lambda-Man version of the game, the first Lambda-Man is the only Lambda-Man.
--
interrupt world state 1 = let (x,y) = lambdaManXY world in do
        writeArray (registers state) A x
        writeArray (registers state) B y
--    INT 2
--        Out:
--            Register A: Second Lambda-Man’s x-ordinate
--            Register B: Second Lambda-Man’s y-ordinate
--
--    Stores the second Lambda-Man’s position in registers A (x-ordinate) and B (y-ordinate). In the single Lambda-Man version of the game, the behaviour of this interrupt is unknown.
interrupt _ _ 2 = return () -- do nothing
--
--    INT 3
--        Out:
--            Register A: this ghost’s index
--
--    Stores the ghost’s index in register A.
interrupt world state 3 = do
        writeArray (registers state) A (myIndex world)
--    INT 4
--        In:
--            Register A: ghost index
--        Out:
--            Register A: indexed ghost’s starting x-ordinate
--            Register B: indexed ghost’s starting y-ordinate
--
--    For the ghost with index read from register A, stores its starting position in registers A (x-ordinate) and B (y-ordinate). If A is not a valid ghost index, does nothing.
interrupt world state 4 = do
        a <- readArray (registers state) A
        if (inRange (bounds (ghostStartCoordinates world)) a)
        then let (x,y) = ghostStartCoordinates world ! a in do
                writeArray (registers state) A x
                writeArray (registers state) B y
        else return ()
--
--    INT 5
--        In:
--            Register A: ghost index
--        Out:
--            Register A: indexed ghost’s current x-ordinate
--            Register B: indexed ghost’s current y-ordinate
--
--    For the ghost with index read from register A, stores its current position in registers A (x-ordinate) and B (y-ordinate). If A is not a valid ghost index, does nothing.
--
interrupt world state 5 = do
        a <- readArray (registers state) A
        if (inRange (bounds (ghostCurCoordinates world)) a)
        then let (x,y) = ghostCurCoordinates world ! a in do
                writeArray (registers state) A x
                writeArray (registers state) B y
        else return ()
--    INT 6
--        In:
--            Register A: ghost index
--        Out:
--            Register A: indexed ghost’s current vitality
--            Register B: indexed ghost’s current direction
--
--    For the ghost with index read from register A, stores its vitality in register A, and its direction in register B.
--    Vitality:
--        0: standard;
--        1: fright mode;
--        2: invisible.
--
--    If A is not a valid ghost index, does nothing.
interrupt world state 6 = do
        a <- readArray (registers state) A
        if (inRange (bounds (ghostVitDir world)) a)
        then let (x,y) = ghostVitDir world ! a in do
                writeArray (registers state) A x
                writeArray (registers state) B y
        else return ()
--
--    INT 7
--        In:
--            Register A: map square x-ordinate
--            Register B: map square y-ordinate
--        Out:
--            Register A: contents of map square
--
--    Stores the current contents of map square with index read from registers A (x-ordinate) and B (y-ordinate) in register A. If the co-ordinates lie outside the defined bounds of the map, stores 0.
--    Contents:
--        0: Wall (#)
--        1: Empty (<space>)
--        2: Pill
--        3: Power pill
--        4: Fruit
--        5: Lambda-Man starting position
--        6: Ghost starting position
--
interrupt world state 7 = do
        a <- readArray (registers state) A
        b <- readArray (registers state) B
        let result = if (inRange (bounds (gameMap world)) (a,b))
                     then gameMap world ! (a,b)
                     else 0 in writeArray (registers state) A result
--    INT 8
--        In:
--            Register PC
--            Register A..H
--
--    Sends the current value of the PC and all registers to an external debug/trace agent.
interrupt _ _ 8 = return ()

doInstruction :: GhostWorldView -> GhcState s -> Instruction -> ST s ()
doInstruction world state ins = case ins of
	MOV d s -> f2 (flip const) d s
	INC d -> f1 (+1) d
	DEC d -> f1 (subtract 1) d
	ADD d s -> f2 (+) d s
	SUB d s -> f2 (-) d s
	MUL d s -> f2 (*) d s
	DIV d s -> catch0 s $ f2 div d s
	AND d s -> f2 (.&.) d s
	OR d s -> f2 (.|.) d s
	XOR d s -> f2 xor d s
	JLT targ x y -> f3 (<) targ x y
	JEQ targ x y -> f3 (==) targ x y
	JGT targ x y -> f3 (>) targ x y
	INT (Const i) -> interrupt world state i
	HLT -> writeSTRef (terminate state) True
	where f1 f d = modifyArgument state d f
	      f2 f d s = do
	        a <- readArgument state s
		modifyArgument state d (`f` a)
	      f3 (#) targ x y = do
	        a <- readArgument state x
		b <- readArgument state y
		if (a#b) then do
		c <- readArgument state targ
		writeArgument state (RegArg PC) c
		else return ()
	      catch0 s rest = do
			a <- readArgument state s
			if a == 0 then
				writeSTRef (terminate state) True
			else
				rest
			

readArgument :: GhcState s -> Argument -> ST s Word8
readArgument state (RegArg reg) = readArray (registers state) reg
readArgument state (IRegArg reg) = do
	ptr <- readArray (registers state) reg
	readArray (mem state) ptr
readArgument _ (Const x) = return x
readArgument state (Memory ptr) = readArray (mem state) ptr

writeArgument :: GhcState s -> Argument -> Word8 -> ST s ()
writeArgument state (RegArg reg) new = writeArray (registers state) reg new
writeArgument state (IRegArg reg) new = do
	ptr <- readArray (registers state) reg
	writeArray (mem state) ptr new
writeArgument _ (Const x) new = undefined -- TODO: Raise proper error
writeArgument state (Memory ptr) new = writeArray (mem state) ptr new

modifyArgument :: GhcState s -> Argument -> (Word8 -> Word8) -> ST s ()
modifyArgument state arg f = readArgument state arg >>= (writeArgument state arg . f)

