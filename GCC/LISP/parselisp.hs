module Lisp.Parse where

import Lisp
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data Prelisp = PList [Prelisp] | Lit String deriving (Show)

parsePrelisp :: String -> Either (ParseError) Prelisp
parsePrelisp = parse (do{l<-parseList;eof;return l}) "unknown"

spaces :: GenParser Char s String
spaces = many (char ' ')

parseList :: GenParser Char s Prelisp
parseList = liftM PList $ spaces >> many1 (do {e<-parseExpr; spaces; return e})

parseExpr :: GenParser Char s Prelisp
parseExpr = (liftM Lit (many1 (oneOf (['0'..'9']++['A'..'Z']++['a'..'z'])))) <|> 
                do{char '('; l<-parseList;char ')';return l}






lispify :: Prelisp -> Expression
lispify (Lit str) = case head (reads str) ::(Int,String) of
                     (n,"")    -> IntLit n
                     _         -> Name str
lispify (PList [])     = List []
lispify (PList [x])    = lispify x
lispify (PList 
