module GCC.Parser where

import GCC.Types

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Int
import qualified Data.Map as M

zeroarity = 
	[("ADD", ADD),
	("SUB", SUB),
	("MUL", MUL),
	("DIV", DIV),
	("CEQ", CEQ),
	("CGT", CGT),
	("CGTE", CGTE),
	("ATOM", ATOM),
	("CONS", CONS),
	("CAR", CAR),
	("CDR", CDR),
	("JOIN", JOIN),
	("RTN", RTN),
	("STOP", STOP)]
onearity  = [
	("LDC", LDC),
	("LDF", LDF),
	("AP", AP),
	("DUM", DUM),
	("RAP", RAP)]
twoarity  = [
	("LD", LD),
	("SEL", SEL)]

gccParser :: GenParser Char s [Instruction]
gccParser = do
	whiteSpace lexer
	ins <- many (lexeme lexer gccInstr)
	eof
	return ins

gccInstr = gccZeroarity <|> gccOnearity <|> gccTwoarity

argument :: GenParser Char s Int32
argument = argconst

argconst = do
	co <- integer lexer
	return $ fromInteger co

gccZeroarity = choice $ map (\ (x, y) -> reserved lexer x >> (return) y) zeroarity
gccOnearity = choice $ map (\ (x, y) -> reserved lexer x >> fmap  y argument) onearity
gccTwoarity = choice $ map (\ (x, y) -> reserved lexer x >> do
	whiteSpace lexer
	a <- argument
	whiteSpace lexer
	b <- argument
	return (y a b)) twoarity

lexer = makeTokenParser gccLanguage

gccLanguage :: LanguageDef st
gccLanguage = LanguageDef { commentStart="",
		commentEnd="",
		commentLine=";",
		nestedComments=False,
		caseSensitive=False,
		identStart=letter,
		identLetter=letter,
		opStart=letter,
		opLetter=letter,
		reservedNames=map fst zeroarity ++ map fst onearity ++ map fst twoarity,
		reservedOpNames=[]
}

parseGcc :: String -> Either (ParseError) [Instruction]
parseGcc = parse gccParser "unknown"

