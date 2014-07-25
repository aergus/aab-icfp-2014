module Ghc.Types where

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
	deriving (Eq, Read, Show)

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
	deriving (Eq, Read, Show)

data GhcState s = GS {
	registers :: STUArray s Register Word8,
	code :: Array Word8 Instruction,
	mem :: STUArray s Word8 Word8,
	counter :: STRef s Integer,
	terminate :: STRef s Bool
}


