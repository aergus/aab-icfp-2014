{- LambdaMan game map parser -}

module Parser where

import Game.Types

import Text.ParserCombinators.Parsec

rawGameMap :: GenParser Char st (GameMap a)
rawGameMap =
    do result <- many rawGameMapLine
       return (GM result)

rawGameMapLine :: GenParser Char st [a]
rawGameMapLine =
    do result <- many element
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
                 '\\' -> LambdaMan undefined
                 '='  -> Ghost undefined)

elementChar :: GenParser Char st Char
elementChar = oneOf " #.o%\\="

parseRawGameMap :: String -> Either ParseError GameMap
parseRawGameMap input = parse rawGameMap "(unknown)" input
