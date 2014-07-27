{-# LANGUAGE DeriveFunctor #-}

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

data Arg a = RegArg Register | IRegArg Register | Const a | Memory a
	deriving (Eq, Read, Show, Functor)

type Argument = Arg Word8

data Instr a = MOV {dest :: a, src :: a} 
                 | INC {dest :: a}
		 | DEC {dest :: a}
		 | ADD {dest :: a, src :: a} 
		 | SUB {dest :: a, src :: a} 
		 | MUL {dest :: a, src :: a} 
                 | DIV {dest :: a, src :: a} 
		 | AND {dest :: a, src :: a} 
		 | OR {dest :: a, src :: a} 
		 | XOR {dest :: a, src :: a} 
		 | JLT {targ :: a, x :: a, y :: a}
		 | JEQ {targ :: a, x :: a, y :: a}
		 | JGT {targ :: a, x :: a, y :: a}
		 | INT {i :: a}
		 | HLT
	deriving (Eq, Read, Show, Functor)

type Instruction = Instr Argument

data GhcState s = GS {
	registers :: STUArray s Register Word8,
	code :: Array Word8 Instruction,
	mem :: STUArray s Word8 Word8,
	counter :: STRef s Integer,
	terminate :: STRef s Bool,
        dir :: STRef s Word8
}

-- world from the view of a ghost
data GhostWorldView = WS {
        lambdaManXY :: (Word8, Word8),
        myIndex :: Word8,
        ghostStartCoordinates :: Array Word8 (Word8, Word8),
        ghostCurCoordinates :: Array Word8 (Word8, Word8),
        ghostVitDir :: Array Word8 (Word8,Word8),
        gameMap :: Array (Word8,Word8) Word8
}
