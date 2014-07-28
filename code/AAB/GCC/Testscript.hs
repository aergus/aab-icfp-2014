{- test GCC code -}

module AAB.GCC.Testscript where

import AAB.GCC.Base
import AAB.GCC.Parser

import System.Environment

testgcc :: IO ()
testgcc = do args <- getArgs
             code <- readFile (args !! 0)
             either
                (putStrLn.("ERROR: "++).show)
                (\x -> (runDebug x []) >> (return ()))
                (parseGcc code)
 
