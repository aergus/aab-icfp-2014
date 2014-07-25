import GCC
import GCC.Types
import qualified Data.IntMap as M
import Data.List

printstate :: State -> String
printstate state = "The current content of the stack is: " ++ (concat.(map (' ':)).(map show).datastack $ state) ++ 
              "\nThe current content of the control stack is: " ++ (concat.(intersperse ",").(map (' ':)).(map show).ctrlstack $ state) ++
              "\nThere are " ++(show . M.size . snd . envchain$state)++ " environment frames with keys " ++(show. M.keys .snd.envchain$state)

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
 SUB,
 LD 1 0,
 AP 1,
 RTN,
 LD 0 0,
 LDC 1,
 ADD,
 LD 1 1,
 AP 1,
 RTN]

 

main = test startstate


test :: State -> IO State
test state = do getLine
                let state' = step testcode1 state
                putStrLn (printstate state')
                if stop state' then return state' else test state'
          
          
