{- LambdaMan game map parser -}

module Parser where

import Types

import Text.ParserCombinators.Parsec

gameMap :: GenParser Char st a -> GenParser Char st (GameMap a)
gameMap fieldParser =
    do result <- many (gameMapLine fieldParser)
       return (GM result)

gameMapLine :: GenParser Char st a -> GenParser Char st [a]
gameMapLine fieldParser =
    do result <- many fieldParser
       char '\n'
       return result

element :: GenParser Char st Element
element =
    do candidate <- elementChar
       return (case candidate of
                 ' '  -> Empty
                 '#'  -> Wall
                 '.'  -> Pill
                 'o'  -> PowerPill
                 '%'  -> Fruit undefined
                 '\\' -> Player undefined
                 '='  -> Monster undefined)

elementChar :: GenParser Char st Char
elementChar = oneOf " #.o%\\="

parseGameMap :: GenParser Char () a -> String -> Either ParseError (GameMap a)
parseGameMap fieldParser input = parse (gameMap fieldParser) "(unknown)" input

parseLMMap :: String -> Either ParseError (GameMap Element)
parseLMMap = parseGameMap element
