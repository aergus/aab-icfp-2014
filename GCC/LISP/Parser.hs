module GCC.Lisp.Parser where

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Int
import qualified Data.Map as M

import Lisp
{-
data Expression = App Expression [Expression] 
            | Name String
            | IntLit Int
            | Cons Expression Expression
            | Lam [String] Expression
            | LamF String [String] Expression
            | Add Expression Expression
            | Sub Expression Expression
            | Mul Expression Expression
            | Div Expression Expression
            | Lt  Expression Expression
            | Lte Expression Expression
            | Gt  Expression Expression
            | Gte Expression Expression
            | Eq  Expression Expression
            | If  Expression Expression Expression
            | Car Expression
            | Cdr Expression
            | List [Expression]
            | LamFApp String [String] Expression [Expression]  --only for compilation
	    deriving (Show)
-}

onearity = [
	("Car",Car),
	("Cdr",Cdr)]
twoarity  = [
	(":",Cons),
	("+",Add),
	("-",Sub),
	("*",Mul),
	("/",Div),
	("<",Lt),
	("<=",Lte),
	(">",Gt),
	(">=",Gte),
	("==",Eq)]
threearity  = [
	("If",If)]

lispParser :: GenParser Char s Expression
lispParser = do
	whiteSpace lexer
	e <- lispExpression
	eof
	return e

lispExpression :: GenParser Char s Expression
lispExpression = lispName <|> lispInt <|> (parens lexer lispTerm)

lispName = do
  id <- identifier lexer
  return $ Name id

lispInt = do
  n <- decimal lexer
  return $ IntLit$ fromInteger $ n

lispTerm = lispOnearity <|> lispTwoarity <|> lispThreearity <|> lispLam <|> lispLamF <|> lispList <|> lispApp

lispLam = do 
  reservedOp lexer "\\" 
  xs <- many $ identifier lexer
  e <- lispExpression
  return $ Lam xs e  

lispLamF = do 
  reservedOp lexer "\\r" 
  f <- identifier lexer
  xs <- many $ identifier lexer
  e <- lispExpression
  return $ LamF f xs e  

lispList = do
  reservedOp lexer "[]"
  xs <- many lispExpression
  return $ List xs

lispApp = do
  f <- lispExpression
  xs <- many lispExpression
  return $ App f xs

lispOnearity = choice $ map (\ (x, y) -> reserved lexer x >> fmap  y lispExpression) onearity

lispTwoarity = choice $ map (\ (x, y) -> reservedOp lexer x >> do
	whiteSpace lexer
	a <- lispExpression
	whiteSpace lexer
	b <- lispExpression
	return (y a b)) twoarity

lispThreearity = choice $ map (\ (x, y) -> reserved lexer x >> do
	whiteSpace lexer
	a <- lispExpression
	whiteSpace lexer
	b <- lispExpression
	whiteSpace lexer
	c <- lispExpression
	return (y a b c)) threearity

lexer = makeTokenParser lispLanguage

lispLanguage :: LanguageDef st
lispLanguage = LanguageDef { commentStart="",
		commentEnd="",
		commentLine=";",
		nestedComments=False,
		caseSensitive=False,
		identStart=letter,
		identLetter=letter,
		opStart=oneOf ":+-*/<=>\\[",
		opLetter=oneOf "=r]",
		reservedNames=map fst onearity ++ map fst threearity,
		reservedOpNames=map fst twoarity ++ ["\\", "\\r", "[]"]
}
