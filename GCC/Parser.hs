module GCC.Parser where

import GCC.Types

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Int
import qualified Data.Map as M

type EInstr = Instr (Either String Int32)
data ELine =  ELine { lbls :: [String], instr :: EInstr }
	deriving Show

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
   labels <- many $ try gccLabel
   inst   <- gccInstr
   return $ ELine labels inst

gccLabel :: GenParser Char s String
gccLabel = do ident <- identifier lexer
              colon lexer
	      return $ ident

gccInstr = gccZeroarity <|> gccOnearity <|> gccTwoarity

argument :: GenParser Char s (Either String Int32)
argument = argident <|> argconst

argident = do
	ident <- identifier lexer
	return $ Left ident

argconst = do
	co <- decimal lexer
	return $ Right $ fromInteger co

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

parseGcc :: String -> Either (ParseError) [ELine]
parseGcc = parse gccParser "unknown"


resolveIdentifier :: [ELine] -> [Instr Int32]
resolveIdentifier el = let m = M.fromList $ concat $ zipWith (\a b -> zip a (repeat b)) (map lbls el) [0..]
		           f (Right x) = x
		           f (Left y)  = m M.! y
                       in map (fmap f. instr) el

