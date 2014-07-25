module GCC.Interface where
import GCC



class ToCons a where
 toCons :: a -> DataValue

instance (ToCons a, ToCons b) => ToCons (a,b) where
 toCons (x,y) = TAG_CONS (toCons x) (toCons y)
instance (ToCons a, ToCons b, ToCons c) => ToCons (a,b,c) where
 toCons (x,y,z) = toCons (x,(y,z))
instance (ToCons a, ToCons b, ToCons c,ToCons d) => ToCons (a,b,c,d) where
 toCons (x,y,z,v) = toCons (x,(y,z,v))
instance (ToCons a, ToCons b, ToCons c,ToCons d, ToCons e) => ToCons (a,b,c,d,e) where
 toCons (x,y,z,v,w) = toCons (x,(y,z,v,w))
instance (ToCons a) => ToCons [a] where
 toCons [] = TAG_INT 0
 toCons (x:xs) = TAG_CONS (toCons x) (toCons xs)

type Map = [[Int]]
type LManState = (Int,(Int,Int),Int,Int,Int)
type GhostState = (Int,(Int,Int),Int)
type FruitState = Int


type AIState = ()
type WorldState = (Map,LManState,GhostState,FruitState)

mainCall :: Code -> WorldState -> () -> (DataValue, DataValue -> WorldState -> (DataValue,Int))
mainCall code wstate () = case run code 0 [toCons wstate, TAG_INT 0] of
                           TAG_CONS aistate (TAG_CLOSURE n e) -> (aistate,step n e)
                           _                        -> error "Illegal return type of main"
                             where step n e aistate' wstate' = case run code n [aistate',toCons wstate] of --need to add version of run with env chain!!!
                                              TAG_CONS aistate'' (TAG_INT move) -> (aistate'',move)
                                              _      -> error "Illegal return type of main"    


startstate :: Int -> [DataValue] -> State
startstate entrypoint xs = State [] [TAG_STOP] ([1..],fromList [(0,Environment xs (length xs) False (error "root has no parent"))]) entrypoint 0 False

run :: Code -> Int -> [Value] -> Value 
run code entrypoint xs = run' (startstate entrypoint xs) where
                         run' state = let state' = step code state in
                                       if stop state' then head.datastack$state' else run' state'
