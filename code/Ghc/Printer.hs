module Ghc.Printer where

import Ghc.Types

prettyReg :: Register -> String
prettyReg x = (map (:[]) ['a'..'h']++["pc"]) !! (fromEnum x)

prettyArg :: Argument -> String
prettyArg (Const x) = show x
prettyArg (RegArg x) = prettyReg x
prettyArg (Memory x) = '[':(show x)++"]"
prettyArg (IRegArg x) = '[':(prettyReg x)++"]"

prettyInstr :: Instruction -> String
prettyInstr HLT = "HLT"
prettyInstr (INC x) = "INC" ++ " " ++ prettyArg x
prettyInstr (DEC x) = "DEC" ++ " " ++ prettyArg x
prettyInstr (INT x) = "INT" ++ " " ++ prettyArg x
prettyInstr (MOV x y) = "MOV" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y
prettyInstr (ADD x y) = "ADD" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y
prettyInstr (SUB x y) = "SUB" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y
prettyInstr (MUL x y) = "MUL" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y
prettyInstr (DIV x y) = "DIV" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y
prettyInstr (AND x y) = "AND" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y
prettyInstr (OR x y) = "OR" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y
prettyInstr (XOR x y) = "XOR" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y
prettyInstr (JLT x y z) = "JLT" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y ++ ", " ++ prettyArg z
prettyInstr (JEQ x y z) = "JEQ" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y ++ ", " ++ prettyArg z
prettyInstr (JGT x y z) = "JGT" ++ " " ++ prettyArg x ++ ", " ++ prettyArg y ++ ", " ++ prettyArg z

prettyProgram :: [Instruction] -> String
prettyProgram = unlines . map prettyInstr
