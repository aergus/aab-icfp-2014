{- data types for game logic -}

module Game.Types where

import Game.Dummy

import Data.Array.ST
import Data.Ix
import Data.STRef

newtype GameMap = GM [[Element]]

instance Show GameMap where
    show (GM tss) = foldr (\ ts -> ((showLikeString ts ++ "\n") ++)) "" tss
        where showLikeString = foldr (\ t -> ((show t) ++)) ""

data Element =
    Empty | Wall | Pill | PowerPill | Fruit | LambdaMan LambdaMan | Ghost Ghost

instance Show Element where
    show Empty         = " "
    show Wall          = "#"
    show Pill          = "."
    show PowerPill     = "o"
    show Fruit         = "%"
    show (LambdaMan _) = "\\"
    show (Ghost _)     = "="


isWall :: Element -> Bool
isWall Wall = True
isWall _ = False

isPill :: Element -> Bool
isPill Pill = True
isPill _    = False

isPowerPill :: Element -> Bool
isPowerPill PowerPill = True
isPowerPill _         = False

isFruit :: Element -> Bool
isFruit Fruit = True
isFruit _     = False

isEatable :: Element -> Bool
isEatable e = isPill e || isPowerPill e || isFruit e

-- TODO: add correct fruit points
fruitPoints :: Integer -> Integer
fruitPoints _ = 100

data Fruit = Fr { fActive :: Bool, fFlavour :: Flavour }

data Flavour =
    Cherry | Strawberry | Peach | Apple | Grapes | Galaxian | Bell | Key

data Agent = Ag {
    initPos  :: (Integer, Integer),
    initDir  :: Integer,
    curPos   :: (Integer, Integer),
    curDir   :: Integer,
    primTPM  :: Integer,
    secTPM   :: Integer,
    fast     :: Bool,
    nextMove :: Integer
}

data LambdaMan = LM {
    lAgent       :: Agent,
    lIndex       :: LambdaIndex,
    lCode        :: LambdaManCode,
    lPowerPill   :: Maybe Integer,
    lGhostsEaten :: Maybe Integer,
    lLives       :: Integer,
    lScore       :: Integer
}

data Ghost = Gh {
    gAgent      :: Agent,
    gIndex      :: GhostIndex,
    gCode       :: GhostCode,
    gVisible    :: Bool,
    gFrightened :: Bool
}

data LambdaIndex = LOne | LTwo deriving (Eq, Ord, Ix)

data GhostIndex = GOne | GTwo | GThree | GFour deriving (Eq, Ord, Ix)

data GameState s = GS {
    ticks      :: STRef s Integer,
    gameMap    :: STArray s (Integer, Integer) Element,
    lambdaMen  :: STArray s LambdaIndex (STRef s LambdaMan),
    ghosts     :: STArray s GhostIndex (STRef s Ghost),
    frightMode :: STRef s (Maybe Integer),
    fruitState :: STRef s Bool,
    pillCount  :: STRef s Integer,
    mapDims    :: (Integer, Integer),
    level      :: Integer
 }
