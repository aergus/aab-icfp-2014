{- test GCC code -}

module GCC.Testscript where

import GCC.Test
import GCC.Parser

import System.Environment

testgcc :: IO ()
testgcc = do args <- getArgs
             code <- readFile (args !! 0)
             either
                (putStrLn.("ERROR: "++).show)
                (\x -> (runtest.(map (fmap fromIntegral)).resolveIdentifier $ x) >> (return ()))
                (parseGcc code)
