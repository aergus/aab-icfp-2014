module AAB.GCC.EnvFrame where

import AAB.GCC.Types
import qualified Data.IntMap as M
import Data.Int
import Data.List

data EnvFrame      = EnvFrame {values :: [DataValue],
                                      size :: Int,
                                      dummy :: Bool,
                                     parent :: Key,
                                        refs :: Int} deriving (Show)


type Key = Int


data EnvChain = EnvChain {freeKeys :: [Key],
                         envframes :: M.IntMap EnvFrame,
                           current :: Key}

instance Show EnvChain where
 show (EnvChain{envframes = xs, current = k}) = "EnvChain {envframes = "++(show xs)++", current = "++(show k)++"}"
                          
                            
currentFrame :: EnvChain -> EnvFrame
currentFrame env = case M.lookup (current env) (envframes env) of
                    (Just x) -> x
                    Nothing  -> error "GCC.EnvFrame.currentFrame"

-- count referenced keys
valRefs :: DataValue -> M.IntMap Int
valRefs (TAG_INT _) = M.fromList []
valRefs (TAG_CLOSURE _ k) = M.fromList [(k,1)]
valRefs (TAG_CONS v1 v2) = M.unionWith (+) (valRefs v1) (valRefs v2)



-- functions working on EnvChain

-- eats all frames f such that f and all descendants of f are not referenced, until there's no more to delete.
-- They can be referenced by elts of stack and elts of referenced envframes, and by being current.
gc :: EnvChain -> EnvChain
gc env = let (freedKeys,newframes) = gc' [] (envframes env) in
          env {freeKeys = freedKeys ++ (freeKeys env), envframes=newframes} where
           gc' ks frames = let (del,rest) = M.partition ((==0).refs) frames in
                       case unzip . M.toAscList $ del of
                        ([],[]) -> (ks,rest)
                        (freedkeys,delframes) -> gc' (freedkeys++ks) 
                                                  (forgetListRef (concat (map values delframes)) rest)

-- loads j'th value of i'th parent frame of current one
ld :: Int32 -> Int32 -> EnvChain -> Maybe DataValue
ld i j env = ld' i j (current env) (envframes env) where
             ld' 0 j1 k frames = case M.lookup k frames of
                                 Just envframe -> Just $ (values envframe) !! (fromIntegral j1)   
                                 Nothing -> Nothing
             ld' i1 j1 k frames = case M.lookup k frames of
                                 Just envframe -> ld' (i-1) j1 (parent envframe) frames
                                 Nothing -> Nothing


-- creates frame of size i
newframe :: Int32 -> EnvChain -> EnvChain
newframe i env = let (k:rest) = freeKeys env in
                 let newFrame = EnvFrame {values=[],size=fromIntegral i,dummy=True,refs=0, parent=(current env)} in
                   setEnv k (env {envframes = M.insert k newFrame (envframes env), freeKeys = rest})

-- sets elements of current frame to xs. ONLY call in AP and RAP, since they remove an equal amount of refs from the stack.
newframeWith :: [DataValue] -> EnvChain -> EnvChain
newframeWith xs env = let (k:rest) = freeKeys env in
                      let newFrame = EnvFrame {values=xs,size=length xs,dummy=False,refs=0, parent=(current env)} in
                        setEnv k (env {envframes = M.insert k newFrame (envframes env), freeKeys = rest})

fillframeWith :: [DataValue] -> EnvChain -> EnvChain
fillframeWith xs env = let newFrame = (currentFrame env) {values=xs,dummy=False} in
                        env {envframes = M.insert (current env) newFrame (envframes env)}


-- jumps to given frame
setEnv :: Key -> EnvChain -> EnvChain
setEnv k env = let old = current env in
                   (incref (-1) old) . (incref 1 k) $ (env {current=k})

-- checks whether current frame is dummy
isDummy :: EnvChain -> Bool
isDummy env = case M.lookup (current env) (envframes env) of
                (Just envframe) -> dummy envframe
                Nothing    -> error "GCC.EnvFrame.isDummy"


-- increase resp. decrease refs & parent's refs
incref :: Int -> Key -> EnvChain -> EnvChain
incref n k env = env {envframes = incref' n k (envframes env)} where

incref' :: Int -> Key -> M.IntMap EnvFrame -> M.IntMap EnvFrame
incref' n (-1) frames = frames
incref' n k    frames = case M.lookup k frames of
                                    Just envframe -> incref' n (parent envframe) (M.insert k (envframe{refs=(refs envframe)+n}) frames)
                                    Nothing ->error "GCC.EnvFrame.incref"

forgetRef :: DataValue -> EnvChain -> EnvChain
forgetRef x env = foldr (\(k,n) -> incref (-n) k) env (M.toAscList (valRefs x))

insertRef :: DataValue -> EnvChain -> EnvChain
insertRef x env = foldr (\(k,n) -> incref n k) env (M.toAscList (valRefs x))

forgetListRef :: [DataValue] -> M.IntMap EnvFrame -> M.IntMap EnvFrame
forgetListRef xs env = foldr (\(k,n) -> incref' (-n) k) env (M.toAscList (M.unionsWith (+) (map valRefs xs)))



