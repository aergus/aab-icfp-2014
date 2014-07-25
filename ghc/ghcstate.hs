module Ghc.State where

import Data.Word
import Data.Bits
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Array.MArray
import Data.Array.Unboxed

data Register = A | B | C | D | E | F | G | H | PC
	deriving (Read, Show, Enum, Eq, Ord, Ix)

data Argument = RegArg Register | IRegArg Register | Const Word8 | Memory Word8

data Instruction = MOV {dest :: Argument, src :: Argument} 
                 | INC {dest :: Argument}
		 | DEC {dest :: Argument}
		 | ADD {dest :: Argument, src :: Argument} 
		 | SUB {dest :: Argument, src :: Argument} 
		 | MUL {dest :: Argument, src :: Argument} 
                 | DIV {dest :: Argument, src :: Argument} 
		 | AND {dest :: Argument, src :: Argument} 
		 | OR {dest :: Argument, src :: Argument} 
		 | XOR {dest :: Argument, src :: Argument} 
		 | JLT {targ :: Argument, x :: Argument, y :: Argument}
		 | JEQ {targ :: Argument, x :: Argument, y :: Argument}
		 | JGT {targ :: Argument, x :: Argument, y :: Argument}
		 | INT {i :: Argument}
		 | HLT

data GhcState s = GS {
	registers :: STUArray s Register Word8,
	code :: Array Word8 Instruction,
	mem :: STUArray s Word8 Word8,
	counter :: STRef s Integer,
	terminate :: STRef s Bool
}

loadInstruction :: GhcState s -> ST s ()
loadInstruction state = do
	pc <- readArray (registers state) PC
	doInstruction state (code state ! pc)

doInstruction :: GhcState s -> Instruction -> ST s ()
doInstruction state ins = case ins of
	MOV d s -> f2 (flip const) d s
	INC d -> f1 (+1) d
	DEC d -> f1 (subtract 1) d
	ADD d s -> f2 (+) d s
	SUB d s -> f2 (-) d s
	MUL d s -> f2 (*) d s
	DIV d s -> catch0 s $ f2 div d s -- TODO: Error 
	AND d s -> f2 (.&.) d s
	OR d s -> f2 (.|.) d s
	XOR d s -> f2 xor d s
	JLT targ x y -> f3 (<) targ x y
	JEQ targ x y -> f3 (==) targ x y
	JGT targ x y -> f3 (>) targ x y
	INT i -> undefined -- TODO
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

