module GCC.LISP.Test where

import Game.Types (GameMap (..))
import Game.Test

testLLMap :: String
testLLMap = mapToLLMap testMap

newtype RawString = RS String

instance Show RawString where
  show (RS s) = s

mapToLLMap :: GameMap -> String
mapToLLMap (GM xxs) = toLLList (map (RS . toLLList . (map fromElement)) xxs)

toLLList :: Show a => [a] -> String
toLLList = foldr (\ x s -> "(: " ++ show x ++ " " ++ s ++ ")") "0"
