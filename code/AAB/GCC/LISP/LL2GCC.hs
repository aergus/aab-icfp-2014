{- compile LambdaLisp to GCC code -}

module AAB.GCC.LISP.LL2GCC where

import AAB.GCC.LISP.Base
import AAB.GCC.LISP.Parser

import System.Environment

ll2gcc :: IO ()
ll2gcc = do args <- getArgs
            lispCode <- readFile (args !! 0)
            either
                (\x -> putStrLn $ "ERROR: " ++ show x)
                (\x -> mapM_ putStrLn $ map printLInstr (compile x))
                (parseLisp lispCode)


ll2gccL :: IO ()
ll2gccL = do args <- getArgs
             lispCode <- readFile (args !! 0)
             either                
                (\x -> putStrLn $ "ERROR: " ++ show x)
                (\x -> putStr $ printCodeWithLabels (compileWithLabels x))
                (parseLisp lispCode)
