{- data types for game logic -}

module Types where

import Dummy

import Data.Array.ST
import Data.Ix
import Data.STRef


newtype GameMap = GM [[Element]]

instance Show GameMap where
    show (GM tss) = foldr (\ ts -> ((showLikeString ts ++ "\n") ++)) "" tss
        where showLikeString = foldr (\ t -> ((show t) ++)) ""

data Element =
    Empty | Wall | Pill | PowerPill | Fruit Fruit |
    LambdaMan LambdaMan | Ghost Ghost

instance Show Element where
    show Empty         = " "
    show Wall          = "#"
    show Pill          = "."
    show PowerPill     = "o"
    show (Fruit _)     = "%"
    show (LambdaMan _) = "\\"
    show (Ghost _)     = "="

isPowerPill :: Element -> Bool
isPowerPill PowerPill = True
isPowerPill _         = False

data Fruit = Fr { fActive :: Bool, fFlavour :: Flavour }

data Flavour =
    Cherry | Strawberry | Peach | Apple | Grapes | Galaxian | Bell | Key

data Agent = Ag {
    initPos  :: (Integer, Integer),
    initDir  :: Integer,
    curPos   :: (Integer, Integer),
    curDir   :: Integer,
    tpm      :: Integer,
    nextMove :: Integer
}

data LambdaMan = LM {
    lAgent       :: Agent,
    lIndex       :: LambdaIndex,
    lCode        :: LambdaManCode,
    lGhostsEaten :: Maybe Integer,
    lLives       :: Integer,
    lPoints      :: Integer
}

data Ghost = Gh {
    gAgent     :: Agent,
    gIndex     :: GhostIndex,
    gCode      :: GhostCode,
    gVisible   :: Bool
}

data LambdaIndex = LOne | LTwo deriving Enum

data GhostIndex = GOne | GTwo | GThree | GFour deriving Enum

data GameState s = GS {
    ticks      :: STRef s Integer,
    gameMap    :: STUArray s (Int, Int) Element,
    lambdaMen  :: STUArray s LambdaIndex LambdaMan,
    ghosts     :: STUArray s GhostIndex Ghost,
    frightMode :: STRef s (Maybe Integer),
    pillCount  :: STRef s Integer
 }
