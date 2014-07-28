{- LambdaMan game map parser -}

module Game.Parser where

import Game.Types (GameMap (..), Element (..))

import Text.ParserCombinators.Parsec

gameMap :: GenParser Char st GameMap
gameMap =
    do result <- many gameMapLine
       return (GM result)

gameMapLine :: GenParser Char st [Element]
gameMapLine =
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
                 '%'  -> Fruit
                 '\\' -> LambdaManStart
                 '='  -> GhostStart)

elementChar :: GenParser Char st Char
elementChar = oneOf " #.o%\\="

parseGameMap :: String -> Either ParseError GameMap
parseGameMap input = parse gameMap "(unknown)" input
