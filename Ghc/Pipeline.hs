import Ghc.Parser
import Ghc.Printer
import Ghc.Types
import Ghc.State
import Control.Monad
import Data.STRef
import Control.Monad.ST
import Data.Array
import Data.Array.ST

import Ghc.DummyWorld
import Data.Word

pipeline :: String -> String
pipeline = prettyProgram. fst . (\(Right x) -> resolveIdentifier x) . parseGhc

compileFromFile :: String -> IO ()
compileFromFile f = do
        x <- readFile f
        either print (putStr . prettyProgram.fst.resolveIdentifier) (parseGhc x)

debugFromFile :: String -> IO ()
debugFromFile f = do
        x <- readFile f
        case parseGhc x of
                Left err -> print err
                Right x -> (uncurry debug) $ resolveIdentifier x

generateState :: [Instruction] -> ST s (GhcState s)
generateState c = do
        reg <- newArray (minBound, maxBound) 0
        code <- return $ listArray (minBound, maxBound) (c++repeat HLT)
        mem <- newArray (minBound, maxBound) 0
        counter <- newSTRef 0
        terminate <- newSTRef False
        dir <- newSTRef 2
        return $ GS reg code mem counter terminate dir 

prettyPrintEnum :: GhcState s -> (String, Word8) -> ST s String
prettyPrintEnum state (name, num) = do
        value <- readArray (mem state) num
        return $ name ++ " (" ++ show num ++ "): " ++ show value ++ ", "

prettyPrintRegisters :: STUArray s Register Word8 -> ST s String
prettyPrintRegisters r = do
        values <- mapM (readArray r) [A .. PC]
        return $ show $ zip [A .. PC] values

prettyPrintState :: GhcState s -> [(String, Word8)] -> ST s String
prettyPrintState state enums = do
        cnt <- readSTRef (counter state)
        ar <- prettyPrintRegisters (registers state)
        pEnum <- mapM (prettyPrintEnum state) enums
        return $ concat $ [show cnt, show ar] ++ pEnum

debug :: [Instruction] -> [(String, Word8)] -> IO ()
debug code enums = do
        state <- stToIO $ generateState code
        debugLoop state enums

debugLoop state enums = do
        term <- stToIO $ readSTRef (terminate state)
        output <- stToIO $ prettyPrintState state enums
        putStrLn output
        getLine
        unless term $ do
               stToIO $ step dummy state
               debugLoop state enums 
