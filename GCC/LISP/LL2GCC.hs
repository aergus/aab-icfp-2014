{- compile LambdaLisp to GCC code -}

module GCC.LISP.LL2GCC where

import GCC.LISP.Base
import GCC.LISP.Parser

import System.Environment
import Text.Parsec.String

showLn :: Show a => [a]-> String
showLn []     = ""
showLn (x:xs) = show x ++ "\n" ++ showLn xs

ll2gcc :: IO ()
ll2gcc = do args <- getArgs
            lispCode <- parseFromFile lispParser (args !! 0)
            either
                (\x -> putStrLn $ "ERROR: " ++ show x)
                (\x -> putStr $ showLn $ compile x)
                lispCode
