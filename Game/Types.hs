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

data Fruit = Fr { fActive :: Bool, fFlavour :: Flavour }

data Flavour =
    Cherry | Strawberry | Peach | Apple | Grapes | Galaxian | Bell | Key

data LambdaMan = LM {
    lIndex       :: LambdaIndex,
    lCode        :: LambdaManCode,
    lTpm         :: Integer,
    lInitNav     :: NavigationData,
    lNav         :: NavigationData,
    lNextMove    :: Integer,
    lGhostsEaten :: Maybe Integer,
    lLives       :: Integer,
    lPoints      :: Integer
}

data Ghost = Gh {
    gIndex     :: GhostIndex,
    gCode      :: GhostCode,
    gNormalTpm :: Integer,
    gAfraidTpm :: Integer,
    gInitNav   :: NavigationData,
    gNav       :: NavigationData,
    gNextMove  :: Integer,
    gVisible   :: Bool
}

data LambdaIndex = LOne | LTwo deriving Enum

data GhostIndex = GOne | GTwo | GThree | GFour deriving Enum

data NavigationData = ND {
    position  :: (Integer, Integer),
    direction :: Integer
}

data GameState s = GS {
    ticks      :: STRef s Integer,
    gameMap    :: STUArray s (Int, Int) Element,
    players    :: STUArray s LambdaIndex LambdaMan,
    monsters   :: STUArray s GhostIndex Ghost,
    frightMode :: STRef s (Maybe Integer),
    pillCount  :: STRef s Integer
 }
