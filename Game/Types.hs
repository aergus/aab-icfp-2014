{- data types for game logic -}

module Game.Types where

import Game.Dummy

import Data.Array.ST
import Data.Int
import Data.Word
import Data.Ix
import Data.STRef

newtype GameMap = GM [[Element]]

instance Show GameMap where
    show (GM tss) = foldr (\ ts -> ((showLikeString ts ++ "\n") ++)) "" tss
        where showLikeString = foldr (\ t -> ((show t) ++)) ""

data Element =
    Empty | Wall | Pill | PowerPill | Fruit | LambdaManStart | GhostStart

instance Show Element where
    show Empty          = " "
    show Wall           = "#"
    show Pill           = "."
    show PowerPill      = "o"
    show Fruit          = "%"
    show LambdaManStart = "\\"
    show GhostStart     = "="

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

fruitPoints :: Integer -> Integer
fruitPoints 1  = 100
fruitPoints 2  = 300
fruitPoints 3  = 500
fruitPoints 4  = 500
fruitPoints 5  = 700
fruitPoints 6  = 700
fruitPoints 7  = 1000
fruitPoints 8  = 1000
fruitPoints 9  = 2000
fruitPoints 10 = 2000
fruitPoints 11 = 3000
fruitPoints 12 = 3000
fruitPoints _  = 5000

type Tick = Word16
type GInt = Word8
type LInt = Int32

data Agent = Ag {
    initPos  :: (GInt, GInt),
    initDir  :: GInt,
    curPos   :: (GInt, GInt),
    curDir   :: GInt,
    primTPM  :: Tick,
    secTPM   :: Tick,
    fast     :: Bool,
    nextMove :: Tick
}

class IsAgent a where
   getA :: a -> Agent
   modA :: a -> (Agent -> Agent) -> a

data LambdaMan = LM {
    lAgent       :: Agent,
    lCode        :: LambdaManCode,
    lPowerPill   :: Maybe Tick,
    lGhostsEaten :: Maybe Tick,
    lLives       :: LInt,
    lScore       :: LInt
}

instance IsAgent LambdaMan where
    getA       = lAgent
    modA man f = man {lAgent = f (lAgent man)}

data Ghost = Gh {
    gAgent      :: Agent,
    gIndex      :: Word8,
    gCode       :: GhostCode,
    gVisible    :: Bool,
    gFrightened :: Bool
}


instance IsAgent Ghost where
    getA         = gAgent
    modA ghost f = ghost {gAgent = f (gAgent ghost)}

data GameState s = GS {
    ticks      :: STRef s Tick,
    gameMap    :: STArray s (GInt, GInt) Element,
    ghosts     :: STUArray s GInt Ghost,
    lambdaMan  :: STRef s LambdaMan,
    frightMode :: STRef s (Maybe Tick),
    pillCount  :: STRef s Tick,
    fruitState :: STRef s Bool,
    fruitPos   :: (GInt, GInt),
    mapDims    :: (GInt, GInt),
    level      :: Word8
 }
