module Ghc.Parser where

import Ghc.Types

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Control.Monad

reg = zip (map (:[]) ['a'..'h']++["pc"]) [A .. PC]

zeroarity = [("HLT", HLT)]
onearity  = [("INC", INC, [noPC]),
             ("DEC", DEC, [noPC]),
	     ("INT", INT, [argument])]
twoarity  = [("MOV", MOV, [argument, argument]),
             ("ADD", ADD, [noPC, argument]),
             ("SUB", SUB, [noPC, argument]),
             ("MUL", MUL, [noPC, argument]),
             ("DIV", DIV, [noPC, argument]),
             ("AND", AND, [noPC, argument]),
             ("OR", OR, [noPC, argument]),
             ("XOR", XOR, [noPC, argument])]
threearity= [("JLT", JLT, [argument, argument, argument]),
             ("JEQ", JEQ, [argument, argument, argument]),
             ("JGT", JGT, [argument, argument, argument])]

ghcParser :: GenParser Char s [Instruction]
ghcParser = do
	whiteSpace lexer
	ins <- many (lexeme lexer ghcInstruction)
	eof
	return ins

argument :: GenParser Char s Argument
argument = do
   ghcReg
   <|> ghcIReg
   <|> ghcCon
   <|> ghcMem

noConst :: GenParser Char s Argument
noConst = do
	 p <- argument
         case p of
		  Const _ -> fail "no const allowed"
		  _ -> return p

noPC :: GenParser Char s Argument
noPC = do
     p <- noConst
     when (p == RegArg PC)$ fail "no pc allowed"
     return p

ghcReg = choice $ map (\ (x,y) -> reserved lexer x >> return (RegArg y)) reg
ghcIReg = brackets lexer $choice $ map (\ (x,y) -> reserved lexer x >> return (IRegArg y)) reg

ghcCon = fmap (Const . fromInteger) $ decimal lexer
ghcMem = brackets lexer $ fmap (Memory . fromInteger) $ decimal lexer

ghcInstruction :: GenParser Char s Instruction
ghcInstruction = do
	ghcZeroarity
	<|> ghcOnearity
	<|> ghcTwoarity
	<|> ghcThreearity

ghcZeroarity = choice $ map (\ (x, y) -> reserved lexer x >> return y) zeroarity
ghcOnearity = choice $ map (\ (x, y, [z1]) -> reserved lexer x >> fmap y z1) onearity
ghcTwoarity = choice $ map (\ (x, y, [z1,z2]) -> reserved lexer x >> do
	a <- z1
	comma lexer
	b <- z2
	return (y a b)) twoarity
ghcThreearity = choice $ map (\ (x, y, [z1,z2,z3]) -> reserved lexer x >> do
	a <- z1
	comma lexer
	b <- z2
	comma lexer
	c <- z3
	return (y a b c) ) threearity


lexer = makeTokenParser ghcLanguage

ghcLanguage :: LanguageDef st
ghcLanguage = LanguageDef { commentStart="",
		commentEnd="",
		commentLine=";",
		nestedComments=False,
		caseSensitive=False,
		identStart=letter,
		identLetter=letter,
		opStart=letter,
		opLetter=letter,
		reservedNames=map fst reg ++ [
			"MOV",
			"INC",
			"DEC",
			"ADD",
			"SUB",
			"MUL",
			"DIV",
			"AND",
			"OR",
			"XOR",
			"JLT",
			"JEQ",
			"JGT",
			"INT",
			"HLT"],
		reservedOpNames=[]
}

parseGhc :: String -> Either (ParseError) [Instruction]
parseGhc = parse ghcParser "unknown"
