import Ghc.Parser
import Ghc.Printer

pipeline :: String -> String
pipeline = prettyProgram . (\(Right x) -> resolveIdentifier x) . parseGhc

pipelineFromFile :: String -> IO ()
pipelineFromFile f = do
        x <- readFile f
        either print (putStr . prettyProgram.resolveIdentifier) (parseGhc x)
