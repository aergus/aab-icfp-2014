{- data types for game logic -}

module Types where

import Dummy

newtype GameMap a = GM [[a]]

instance Show a => Show (GameMap a) where
    show (GM tss) = foldr (\ ts -> ((showLikeString ts ++ "\n") ++)) "" tss
        where showLikeString = foldr (\ t -> ((show t) ++)) ""

data Element =
    Empty | Wall | Pill | PowerPill | Fruit Flavour |
    Player LambdaMan | Monster Ghost

instance Show Element where
    show Empty       = " "
    show Wall        = "#"
    show Pill        = "."
    show PowerPill   = "o"
    show (Fruit _)   = "%"
    show (Player _)  = "\\"
    show (Monster _) = "="

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

data GameState a = GS {
    ticks      :: Integer,
    map        :: GameMap a,
    players    :: [LambdaMan],
    monsters   :: [Ghost],
    frightMode :: Bool
}
