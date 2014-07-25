{- data types for game logic -}

module Types where

import Dummy

newtype GameMap = GM [[Element]]

data Element =
    Empty | Wall | Pill | PowerPill | Fruit Flavour |
    Player LambdaMan | Monster Ghost

data Flavour =
    Cherry | Strawberry | Peach | Apple | Grapes | Galaxian | Bell | Key

data Ghost = GH {
  index     :: Integer,
  ghostPos  :: (Integer, Integer),
  normalTpm :: Integer,
  afraidTpm :: Integer,
  ghostCode :: GhostCode
}

data LambdaMan = LM {
  points      :: Integer,
  lambdaPos   :: (Integer, Integer),
  lambdaTpm   :: Integer,
  ghostsEaten :: Maybe Integer,
  lambdaCode  :: LambdaManCode
}

data Position        = Pos { x :: Integer, y :: Integer }

data GameState = GS {
    ticks      :: Integer,
    map        :: GameMap,
    players    :: [LambdaMan],
    monsters   :: [Ghost],
    frightMode :: Bool
}
