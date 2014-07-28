module AAB.GHC.Pipeline where

import AAB.GHC.Parser
import AAB.GHC.Printer
import AAB.GHC.Types
import AAB.GHC.State
import Control.Monad
import Data.STRef
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List

import Text.Parsec.Pos

import AAB.GHC.Test
import Data.Word

pipeline :: String -> String
pipeline = prettyProgram. instrs . (\(Right x) -> resolveIdentifier x) . parseGhc

compileFromFile :: String -> IO String
compileFromFile f = do
        x <- readFile f
        case (parseGhc x) of
                (Left y) -> return $ show y
                (Right y) -> return $ prettyProgram $ instrs $ resolveIdentifier $ y

debugFromFile :: String -> IO ()
debugFromFile f = do
        x <- readFile f
        case parseGhc x of
                Left err -> print err
                Right y -> debug x $ resolveIdentifier y

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

context :: String -> GhcState s -> ParseResult -> ST s [String]
context source state pr = do
        pc <- fmap( fromInteger . toInteger) $ readArray (registers state) PC
        let line = sourceLine (positions pr !! pc)
        return $ take 10 $ drop (line - 4) $lines source 

prettyPrintState :: String -> GhcState s -> ParseResult -> ST s String
prettyPrintState source state pr = do
        cnt <- readSTRef (counter state)
        ar <- prettyPrintRegisters (registers state)
        pEnum <- mapM (prettyPrintEnum state) (enums pr)
        ctxt <- context source state pr
        return $ intercalate "\n" $ [show ar] ++ pEnum ++ ctxt ++ [show cnt]

debug :: String -> ParseResult -> IO ()
debug source pr = do
        state <- stToIO $ generateState $ instrs $ pr
        debugLoop source state pr

debugLoop source state pr = do
        term <- stToIO $ readSTRef (terminate state)
        output <- stToIO $ prettyPrintState source state pr
        putStrLn output
        getLine
        unless term $ do
               stToIO $ step testWorld state
               debugLoop source state pr 
