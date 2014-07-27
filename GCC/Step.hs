module GCC.Step where
import GCC.Types
import Data.Int
import GCC.EnvFrame
import GCC.State













              
step :: State -> Either String State
step state = case getInstr state of
 (LDC n)  -> Right . incCP . (pushData (TAG_INT (fromIntegral n)))   $ state
 (LD fr i)-> case ld fr i (envchain state) of
                Just val    -> Right . incCP . (pushData val) $ state{envchain = insertRef val (envchain state)} 
                Nothing     -> Left "Illegal frame"
 (LDF f)  -> let k = current (envchain state) in 
                 Right . incCP . (pushData (TAG_CLOSURE (fromIntegral f) k))   $ state{envchain=incref 1 (fromIntegral k) (envchain state)}
 ADD      -> case pop2 (datastack state) of
               Just (TAG_INT y, TAG_INT x, rest) -> Right . incCP . pushData (TAG_INT (x+y)) $ state{datastack=rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 SUB      -> case pop2 (datastack state) of
               Just (TAG_INT y, TAG_INT x, rest) -> Right . incCP . pushData (TAG_INT (x-y)) $ state{datastack=rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 MUL      -> case pop2 (datastack state) of
               Just (TAG_INT y, TAG_INT x, rest) -> Right . incCP . pushData (TAG_INT (x*y)) $ state{datastack=rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 DIV      -> case pop2 (datastack state) of
               Just (TAG_INT 0, TAG_INT x, rest) -> Right . incCP                            $ state{datastack=rest}
               Just (TAG_INT y, TAG_INT x, rest) -> Right . incCP . pushData (TAG_INT (x `div` y)) $ state{datastack=rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 CEQ      -> case pop2 (datastack state) of
               Just (TAG_INT y, TAG_INT x, rest) -> Right . incCP . pushData (TAG_INT (if x==y then 1 else 0)) $ state{datastack=rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 CGT      -> case pop2 (datastack state) of
               Just (TAG_INT y, TAG_INT x, rest) -> Right . incCP . pushData (TAG_INT (if x>y  then 1 else 0)) $ state{datastack=rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 CGTE     -> case pop2 (datastack state) of
               Just (TAG_INT y, TAG_INT x, rest) -> Right . incCP . pushData (TAG_INT (if x>=y then 1 else 0)) $ state{datastack=rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 CONS     -> case pop2 (datastack state) of
               Just (y,x,rest)                   -> Right . incCP . pushData (TAG_CONS x y) $ state{datastack=rest}
               Nothing                           -> Left "Stack empty"
 CAR      -> case pop (datastack state) of
               Just (TAG_CONS x y,rest)          -> Right . incCP . pushData x 
                                                      $ state{datastack=rest,envchain = forgetRef y (envchain state)}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 CDR      -> case pop (datastack state) of
               Just (TAG_CONS x y,rest)          -> Right . incCP . pushData y 
                                                      $ state{datastack=rest,envchain = forgetRef x (envchain state)}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 ATOM     -> case pop (datastack state) of
               Just (TAG_INT _,rest)                     -> Right . incCP . pushData (TAG_INT 1) 
                                                                           $ state{datastack=rest}
               Just (x,rest)                     -> Right . incCP . pushData (TAG_INT 0)
                                                                           $ state{datastack=rest, envchain=forgetRef x (envchain state)} 
               _                                 -> Left "Stack empty"
 (DUM l)  -> Right . incCP $ state{envchain = newframe l (envchain state)}
 (AP l)   -> case poplist (fromIntegral (l+1)) (datastack state) of
               Just ((TAG_CLOSURE f k):xs,rest)  -> let env' = newframeWith (reverse xs) (setEnv k (envchain state)) in
                                                    let old = current.envchain$ state in
                                                        Right . (goto (fromIntegral f)) .
                                                        (pushCtrl (TAG_RET ((+1).cp.codemap$ state))). 
                                                        (pushCtrl (TAG_FRAME old))
                                                        $ state{envchain = ((incref 1 old).(incref (-1) (fromIntegral k))$ env'), datastack = rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 (RAP l)   -> case poplist (fromIntegral (l+1)) (datastack state) of
               Just ((TAG_CLOSURE f k):xs,rest)  -> if rapFault k l state then Left "Frame mismatch" else 
                                                    let env' = fillframeWith (reverse xs) (envchain state) in
                                                    let oldparent = parent.currentFrame.envchain$ state in
                                                        Right . (goto (fromIntegral f)) .
                                                        (pushCtrl (TAG_RET ((+1).cp.codemap$ state))). 
                                                        (pushCtrl (TAG_FRAME oldparent))
                                                        $ state{envchain = ((incref 1 oldparent).(incref (-1) (fromIntegral k))$ env'), datastack = rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 RTN       -> case pop (ctrlstack state) of
               Just (TAG_STOP,_) -> Right. halt $ state
               _ -> case pop2 (ctrlstack state) of
                     Just (TAG_RET addr,TAG_FRAME k,rest) -> Right . (goto (fromIntegral addr)) $ 
                                                                        state{envchain = (incref (-1) (fromIntegral k)).
                                                                              (setEnv (fromIntegral k)) $ (envchain state)}
                     Just _                               -> Left "Tag mismatch"
                     Nothing                              -> Left "Stack empty"
 (SEL t f) -> case pop (datastack state) of
               Just (TAG_INT n,rest)      -> case n of
                                               0 -> Right . (goto (fromIntegral f)) . (pushCtrl (TAG_JOIN ((+1).cp.codemap$ state))) $ state{datastack=rest} 
                                               _ -> Right . (goto (fromIntegral t)) . (pushCtrl (TAG_JOIN ((+1).cp.codemap$ state))) $ state{datastack=rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 JOIN      -> case pop (ctrlstack state) of
               Just (TAG_JOIN addr,rest)         -> Right . (goto (fromIntegral addr)) $ state{ctrlstack=rest}
               Just _                            -> Left "Tag mismatch"
               Nothing                           -> Left "Stack empty"
 STOP      ->  Right $ halt state              

