import Ghc.Parser
import Ghc.Printer

pipeline :: String -> String
pipeline = prettyProgram . (\(Right x) -> resolveIdentifier x) . parseGhc
