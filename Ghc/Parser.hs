module Ghc.Parser where

import Ghc.Types

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Control.Monad
import Data.Word
import qualified Data.Map as M

type EArg = Arg (Either String Word8)
type EInstr = Instr EArg
data ELine = Line {def :: [(String, Word8)], defEnum :: [String], lbls :: [String], instr :: EInstr, pos :: SourcePos}
        deriving (Show)

reg = zip (map (:[]) ['a'..'h']++["pc"]) [A .. PC]

zeroarity = [("HLT", HLT)]
onearity  = [("INC", INC, [noPC]),
             ("DEC", DEC, [noPC]),
             ("INT", INT, [ghcCon])]
twoarity  = [("MOV", MOV, [argument, argument]),
             ("ADD", ADD, [noPC, argument]),
             ("SUB", SUB, [noPC, argument]),
             ("MUL", MUL, [noPC, argument]),
             ("DIV", DIV, [noPC, argument]),
             ("AND", AND, [noPC, argument]),
             ("OR", OR, [noPC, argument]),
             ("XOR", XOR, [noPC, argument])]
threearity= [("JLT", JLT, [ghcCon, argument, argument]),
             ("JEQ", JEQ, [ghcCon, argument, argument]),
             ("JGT", JGT, [ghcCon, argument, argument])]

ghcParser :: GenParser Char s [ELine]
ghcParser = do
        whiteSpace lexer
        ins <- many (lexeme lexer ghcLine)
        eof
        return ins

argument :: GenParser Char s EArg
argument = do
   ghcReg
   <|> try ghcIReg
   <|> ghcCon
   <|> ghcMem

noConst :: GenParser Char s EArg
noConst = do
         p <- argument
         case p of
                  Const _ -> fail "no const allowed"
                  _ -> return p

noPC :: GenParser Char s EArg
noPC = do
     p <- noConst
     when (p == RegArg PC)$ fail "no pc allowed"
     return p

ghcReg :: GenParser Char s EArg
ghcReg = choice $ map (\ (x,y) -> reserved lexer x >> return (RegArg y)) reg
ghcIReg :: GenParser Char s EArg
ghcIReg = brackets lexer $choice $ map (\ (x,y) -> reserved lexer x >> return (IRegArg y)) reg

ghcEither :: GenParser Char s (Either String Word8)
ghcEither = ghcWord <|> ghcIdent

ghcWord :: GenParser Char s (Either String Word8)
ghcWord = do
  x <- decimal lexer
  whiteSpace lexer
  return $ Right $ fromInteger x

ghcIdent :: GenParser Char s (Either String Word8)
ghcIdent = do
  x <- identifier lexer
  return $ Left x

ghcCon :: GenParser Char s EArg
ghcCon = fmap (Const ) $ ghcEither
ghcMem :: GenParser Char s EArg
ghcMem = brackets lexer $ fmap (Memory) $ ghcEither 

ghcLine :: GenParser Char s ELine
ghcLine = do
  defs <- many ghcDef
  defEnums <- many ghcDefEnum
  lbls <- many $ try ghcLabel
  p <- getPosition
  instr <- ghcInstruction
  return $Line defs defEnums lbls instr p

ghcDef :: GenParser Char s (String, Word8) 
ghcDef = do
        reserved lexer "DEF"
        id <- identifier lexer
        num <- decimal lexer
        whiteSpace lexer
        return (id, fromInteger num)

ghcDefEnum :: GenParser Char s String 
ghcDefEnum = do
        reserved lexer "DefEnum"
        id <- identifier lexer
        return id

ghcLabel :: GenParser Char s String
ghcLabel = do ident <- identifier lexer
              colon lexer
              return ident

ghcInstruction :: GenParser Char s EInstr
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
ghcLanguage = LanguageDef { commentStart="{-",
                commentEnd="-}",
                commentLine=";",
                nestedComments=False,
                caseSensitive=False,
                identStart=letter <|> oneOf "_",
                identLetter=alphaNum <|> oneOf "_",
                opStart=letter,
                opLetter=alphaNum,
                reservedNames=map fst reg ++ [
                        "DEF",
                        "DefEnum",
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

parseGhc :: String -> Either (ParseError) [ELine]
parseGhc = parse ghcParser "unknown"

--TODO: REWRITE
resolveIdentifier :: [ELine] -> ParseResult
resolveIdentifier el = let m1 = concat (zipWith (\a b -> zip a (repeat b)) (map lbls el) [0..])
                           m2 = concat $ map def el
                           m3 = zip (concat $ map defEnum el) [255,254..0]
                           m =M.fromList $ m1 ++ m2 ++ m3
                           f (Right x) = x
                           f (Left y)  = m M.! y
                       in ParseResult (map ( fmap (fmap f). instr) el) m3 (map pos el)
