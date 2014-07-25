module GCC.Parser where

import GCC.Types

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Int

type EInstr = Instr (Either String Int32)
type ELine = Either String EInstr

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

gccParser :: GenParser Char s [ELine]
gccParser = do
	whiteSpace lexer
	ins <- many (lexeme lexer gccELine)
	eof
	return ins

gccELine :: GenParser Char s ELine
gccELine = do
  gccInstr <|> gccLabel

gccLabel :: GenParser Char s ELine
gccLabel = do ident <- identifier lexer
              colon lexer
	      return $ Left ident

gccInstr = gccZeroarity <|> gccOnearity <|> gccTwoarity

argument :: GenParser Char s (Either String Int32)
argument = argident <|> argconst

argident = do
	ident <- identifier lexer
	return $ Left ident

argconst = do
	co <- decimal lexer
	return $ Right $ fromInteger co

gccZeroarity = choice $ map (\ (x, y) -> reserved lexer x >> (return. Right) y) zeroarity
gccOnearity = choice $ map (\ (x, y) -> reserved lexer x >> fmap (Right . y) argument) onearity
gccTwoarity = choice $ map (\ (x, y) -> reserved lexer x >> do
	a <- argument
	b <- argument
	return (Right (y a b))) twoarity

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

parseGcc :: String -> Either (ParseError) [ELine]
parseGcc = parse gccParser "unknown"
