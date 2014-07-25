import GCC
import GCC.Types
import qualified Data.IntMap as M
import Data.List

printstate :: Code -> State -> String
printstate code state = "The current content of the stack is: " ++ (concat.(map (' ':)).(map show).datastack $ state) ++ 
              "\nThe current content of the control stack is: " ++ (concat.(intersperse ",").(map (' ':)).(map show).ctrlstack $ state) ++
              "\nThe environment frames are:\n" ++ (concat.(map (++"\n")).(map (' ':)).(map printE). M.toAscList .snd.envchain$state)
            ++ "Next instruction: "++ (show (code M.! (currentinstr state)))

printE :: (Int,Environment) -> String
printE (n,e) = "<env"++(show n)++">: ["++(concat $ intersperse "," (map show (values e)))++"], "
                    ++(if n==0 then "root" else "parent <env"++(show (parent e))++">" )


instance Show DataValue where
 show (TAG_INT n) = show n
 show (TAG_CONS x y) = "("++(show x)++","++(show y)++")"
 show (TAG_CLOSURE n e) = "{"++(show n)++" "++"<env"++(show e)++">}"

instance Show ControlValue where
 show TAG_STOP = "TAG_STOP"
 show (TAG_RET n) = "TAG_RET "++(show n)
 show (TAG_FRAME e) = "<env"++(show e)++">"
 show (TAG_JOIN n) = "TAG_JOIN "++(show n)

startstate :: State
startstate = State [] [TAG_STOP] newEnvironmentChain 0 0 False

testcode1 :: Code
testcode1 = M.fromList . (zip [0..]) $ [
 DUM 2,
 LDF 16,
 LDF 10,
 LDF 6,
 RAP 2,
 RTN,
 LDC 1,
 LD 0 0,
 AP 1,
 RTN,
 LD 0 0,
 LDC 1,
 ADD,
 LD 1 0,
 AP 1,
 RTN,
 LD 0 0,
 LDC 1,
 ADD,
 LD 1 1,
 AP 1,
 RTN]

 

main = do putStrLn$ "The first instruction is: "++ (show (testcode1 M.! 0))
          test testcode1 startstate


test :: Code -> State -> IO State
test code state = do getLine
                     let state' = step code state
                     putStrLn (printstate code state')
                     if stop state' then return state' else test code state'
          
          
