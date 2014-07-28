{- test GCC code -}

module GCC.Testscript where

import GCC.Base
import GCC.Parser

import System.Environment

testgcc :: IO ()
testgcc = do args <- getArgs
             code <- readFile (args !! 0)
             either
                (putStrLn.("ERROR: "++).show)
                (\x -> (runDebug x []) >> (return ()))
                (parseGcc code)
 
