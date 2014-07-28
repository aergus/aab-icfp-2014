{-# LANGUAGE DeriveFunctor #-}
module GCC.LISP.Base where

import qualified Data.Map as M
import Data.List



type LISP = (Expression,[(String, Expression)])


-- TODO Optimization stuff:
--    could replace [RAP l, RTN] by [TRAP l], [AP l, RTN] by [TAP l], [SEL t f, JOIN] by [TSEL t f] for performance (saves 1 call of ret each)
--    right now all toplevel decs a=exp are turned into a function putting exp on the stack, to be called with AP 0. In case of toplevel
--          a=(\ args -> exp), could instead save function computing exp.
--          a=(\r g args -> exp), can directly simplify to a=(\ args -> exp{subs g by a})


precompile :: LISP -> LabelLCode
precompile (main,defs) = transform $ (prepare main, [(s,prepare exp) | (s,exp) <- defs])

compile :: LISP -> [LInstr Int]
compile = resolveLabels . precompile

printCode :: [LInstr Int] -> String
printCode = concat . (map (++"\n")) . (map show)

printCodeWithLabels :: [(String,[LInstr String])] -> String
printCodeWithLabels = concat . (map (\(label,instrs) -> (label ++ ":\n") ++ (concat.(map (' ':)).(map (++"\n")).(map show) $ instrs)))

compileWithLabels :: LISP -> [(String,[LInstr String])]
compileWithLabels = M.toAscList . precompile

transform :: LISP -> LabelLCode
transform (main,defs) = let l = length defs in
                        let (labels, rest) = splitAt (l+1) rootlabels in
                        let (lvls,call,ctxts,expsToCall) = unzip4 . (map callHow) . (map snd) $ defs in
                        let names = map fst defs in   
                        let ctxt =(M.fromList [(name,(1,i,instr))| (name,i,instr) <- zip3 names [0..] call]) `M.union` (fst firstcontext) in
                         (foldr (<+>) (toCode $ [DUM l]++[LDF s | s <- labels]++[RAP l,RTN])
                          [floatAt s $ (tr (split i rest) (ctxt`M.union` fctxt,lvl) exp)<+>(toCode [RTN])| (s,lvl,exp,i,fctxt) <- zip5 labels lvls expsToCall [1..] ctxts])
                          <+> (floatAt (labels !! l) $ (tr (split 0 rest) (ctxt,1) main)<+>(toCode [RTN])) 

callHow:: Expression -> (Int,[LInstr String],M.Map String (Int,Int,[LInstr Label]),Expression)
callHow (Lam args exp) = (2,[], M.fromList [(name,(2,i,[]))|(name,i) <- zip args [0..]],exp)
callHow x         = (2,[AP 0],M.fromList [],x)

data Expression = App Expression [Expression]
            | Name String
            | IntLit Int
            | Cons Expression Expression
            | Lam [String] Expression
            | LamF String [String] Expression
            | Add Expression Expression
            | Sub Expression Expression
            | Mul Expression Expression
            | Div Expression Expression
            | Lt  Expression Expression
            | Lte Expression Expression
            | Gt  Expression Expression
            | Gte Expression Expression
            | Eq  Expression Expression
            | If  Expression Expression Expression
            | Car Expression
            | Cdr Expression
            | Nil Expression
            | List [Expression]
            | LamFApp String [String] Expression [Expression]  --only for compilation
            | Do [(String, Expression)] Expression        deriving (Show)

freeVariables :: Expression -> [String]
freeVariables (Name s) = [s]
freeVariables (IntLit _) = []
freeVariables (Cons e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (Lam vars e) = (freeVariables e) \\ vars
freeVariables (LamF f vars e) = (freeVariables e) \\ (f:vars)
freeVariables (Add e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (Sub e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (Mul e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (Div e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (Lt e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (Lte e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (Gt e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (Gte e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (Eq e1 e2) = (freeVariables e1) `union` (freeVariables e2)
freeVariables (If e1 e2 e3) = (freeVariables e1) `union` (freeVariables e2) `union` (freeVariables e3)
freeVariables (Car e) = freeVariables e
freeVariables (Cdr e) = freeVariables e
freeVariables (Nil e) = freeVariables e
freeVariables (List exps) = foldr union [] (map freeVariables exps)
freeVariables (App e exps) = foldr union (freeVariables e) (map freeVariables exps)
freeVariables (Do binds exp) = foldr (\(s,e) vs-> (vs \\ [s])`union`(freeVariables e)) (freeVariables exp) binds

prepare1 :: Expression -> Expression --turn LamF into Lam when possible, and desugar do.
prepare1 (LamF f args exp) = case f `elem` (freeVariables exp) of
                                        True -> LamF f args exp
                                        False-> Lam args exp
prepare1 (Car e)           = Car (prepare1 e)
prepare1 (Cdr e)           = Cdr (prepare1 e)
prepare1 (Nil e)           = Nil (prepare1 e)
prepare1 (Add e1 e2)       = Add (prepare1 e1) (prepare1 e2)
prepare1 (Sub e1 e2)       = Sub (prepare1 e1) (prepare1 e2)
prepare1 (Mul e1 e2)       = Mul (prepare1 e1) (prepare1 e2)
prepare1 (Div e1 e2)       = Div (prepare1 e1) (prepare1 e2)
prepare1 (Lt e1 e2)        = Lt (prepare1 e1) (prepare1 e2)
prepare1 (Lte e1 e2)       = Lte (prepare1 e1) (prepare1 e2)
prepare1 (Gt e1 e2)        = Gt (prepare1 e1) (prepare1 e2)
prepare1 (Gte e1 e2)       = Gte (prepare1 e1) (prepare1 e2)
prepare1 (Eq e1 e2)        = Eq (prepare1 e1) (prepare1 e2)
prepare1 (Cons e1 e2)      = Cons (prepare1 e1) (prepare1 e2)
prepare1 (Lam vars e)      = Lam vars (prepare1 e)
prepare1 (List exps)       = List (map prepare1 exps)
prepare1 (If e1 e2 e3)     = If (prepare1 e1) (prepare1 e2) (prepare1 e3)
prepare1 (App e exps)      = App (prepare1 e) (map prepare1 exps)
prepare1 (Do binds exp)    = let bindblocks = sepDo [(s,prepare1 e)|(s,e)<-binds] in
                              foldr (\binds1 exp1 -> App (Lam (map fst binds1) exp1) (map snd binds1)) (prepare1 exp) bindblocks
prepare1 x                 = x


splitDo :: [(String,Expression)] -> ([(String,Expression)],[(String,Expression)])
splitDo xs = f ([],([],xs)) where
                f (_,(ys,[]))  = (ys,[])
                f (vs,(ys,(x:xs))) = case intersect vs (freeVariables.snd$x) of
                                      [] -> f ((fst x):vs,(ys++[x],xs))
                                      _  -> (ys,(x:xs))

sepDo :: [(String,Expression)] -> [[(String,Expression)]] 
sepDo [] = []
sepDo xs = let (block,rest) = splitDo xs in
            block:(sepDo rest)

prepare2 :: Expression -> Expression --turn App (LamF f args exp) exps into LamFApp f args exp exps
prepare2 (App (LamF f args exp) exps) = LamFApp f args (prepare2 exp) (map prepare2 exps)
prepare2 (Car e)           = Car (prepare2 e)
prepare2 (Cdr e)           = Cdr (prepare2 e)
prepare2 (Nil e)           = Nil (prepare2 e)
prepare2 (Add e1 e2)       = Add (prepare2 e1) (prepare2 e2)
prepare2 (Sub e1 e2)       = Sub (prepare2 e1) (prepare2 e2)
prepare2 (Mul e1 e2)       = Mul (prepare2 e1) (prepare2 e2)
prepare2 (Div e1 e2)       = Div (prepare2 e1) (prepare2 e2)
prepare2 (Lt e1 e2)        = Lt (prepare2 e1) (prepare2 e2)
prepare2 (Lte e1 e2)       = Lte (prepare2 e1) (prepare2 e2)
prepare2 (Gt e1 e2)        = Gt (prepare2 e1) (prepare2 e2)
prepare2 (Gte e1 e2)       = Gte (prepare2 e1) (prepare2 e2)
prepare2 (Eq e1 e2)        = Eq (prepare2 e1) (prepare2 e2)
prepare2 (Cons e1 e2)      = Cons (prepare2 e1) (prepare2 e2)
prepare2 (Lam vars e)      = Lam vars (prepare2 e)
prepare2 (LamF f vars e)   = LamF f vars (prepare2 e)
prepare2 (List exps)       = List (map prepare2 exps)
prepare2 (If e1 e2 e3)     = If (prepare2 e1) (prepare2 e2) (prepare2 e3)
prepare2 (App e exps)      = App (prepare2 e) (map prepare2 exps)
prepare2 x                 = x

prepare3 :: [String] -> Expression -> Expression --turn remaining (LamF f args exp) into (Lam args1 (LamFApp f args exp exps))
prepare3 freenames (LamF f args exp)  = let (newvars,rest) = splitAt (length args) freenames in
                                         (Lam newvars (LamFApp f args (prepare3 (rest \\ args) exp) (map Name newvars))) 
prepare3 freenames (Car e)           = Car (prepare3 freenames e)
prepare3 freenames (Cdr e)           = Cdr (prepare3 freenames e)
prepare3 freenames (Nil e)           = Nil (prepare3 freenames e)
prepare3 freenames (Add e1 e2)       = Add (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Sub e1 e2)       = Sub (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Mul e1 e2)       = Mul (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Div e1 e2)       = Div (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Lt e1 e2)        = Lt (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Lte e1 e2)       = Lte (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Gt e1 e2)        = Gt (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Gte e1 e2)       = Gte (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Eq e1 e2)        = Eq (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Cons e1 e2)      = Cons (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2)
prepare3 freenames (Lam vars e)      = Lam vars (prepare3 (freenames \\ vars) e)
prepare3 freenames (List exps)       = List (zipWith (\i e -> prepare3 (split i freenames) e) [0..] exps)
prepare3 freenames (If e1 e2 e3)     = If (prepare3 (split 0 freenames) e1) (prepare3 (split 1 freenames) e2) (prepare3 (split 2 freenames) e3)
prepare3 freenames (App e exps)      = App (prepare3 (split 0 freenames) e) (zipWith (\i e -> prepare3 (split i freenames) e) [1..] exps)
prepare3 _ x                 = x

prepare :: Expression -> Expression
prepare = (prepare3 rootlabels) . prepare2 . prepare1

data LInstr a =     LDC Int      --LDC loads int constant 
                  | LD Int Int   --LD n i loads i'th value in n'th frame
                  | ADD          --int add
                  | SUB          --int sub
                  | MUL          --int mult
                  | CEQ          --equal
                  | CGT          --greater than
                  | DIV          --int div
                  | CGTE         --greater than or equal
                  | ATOM         --checks whether top elt of the stack is int, consumes it
                  | CONS         --builds cons cell of top two elements of stack, pushes it
                  | CAR          --extracts first element of cons cell
                  | CDR          --extracts second element of cons cell
                  | SEL a a  --sel $t $f pops first elt of stack, if nonzero, go to $t, else to $f. push return adress on stack.
                  | JOIN         --pops first elt, if its an return adress, jump to it.
                  | LDF a      --give literal function address, build closure pointing to that function & with current environment frame
                  | AP Int      --AP n: pop closure; allocate new environment (child of the current one), fill it with n arguments from the stack. Push stack pointer, environment pointer, return address, then jump into function specified in closure and set env to new frame.
                  | RTN          --pop stack pointer, ret adress, env, restore stack and env, jump to ret adress.
                  | DUM Int      --creates empty env frame child with specified size, sets it as current.
                  | RAP Int     --RAP n: pop closure cell. Fill empty frame (pointed to by closure?) with n arguments from the stack. Push stack, parent of current environment, and return address. Set environment frame pointer to frame pointer from closure cell. Jump to code(closure).
                  | STOP deriving (Show, Functor)

printLInstr :: LInstr Int -> String
printLInstr (LDC n) = if n<0 then "LDC -"++(show (-n)) else "LDC "++(show n)
printLInstr x       = show x

type Label = String

rootlabels :: [Label]
rootlabels = (map('/':).concat.(map f))$[1..] where
                  f 0 = [""]
                  f n = [c:s | c <- ['a'..'z'], s <- f (n-1)]

split :: Int -> [Label] -> [Label]
split n = map (++("/"++(show n)))

type LabelLCode = M.Map Label [LInstr Label] --main entry point is label "".

toCode :: [LInstr Label] -> LabelLCode
toCode xs = M.fromList [("",xs)]
 
(<+>) :: LabelLCode -> LabelLCode -> LabelLCode  --assume collision free labels. (can we??)
(<+>) = M.unionWithKey (\k -> if k=="" then (++) else error "label collision")

floatAt :: Label -> LabelLCode -> LabelLCode
floatAt l = M.mapKeys (\x -> if x == "" then l else x) 
           
type Context = (M.Map String (Int,Int,[LInstr Label]),Int) --how deep is each variable, how to call , and how deep are we?

firstcontext :: Context
firstcontext = (M.fromList [("WORLDSTATE",(0,0,False)),("GHOSTCODE",(0,1,False))],0) --the two arguments of main

tr :: [Label] -> Context -> Expression -> LabelLCode
tr ls ctxt (Add e1 e2)   = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [ADD])
tr ls ctxt (Sub e1 e2)   = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [SUB])
tr ls ctxt (Mul e1 e2)   = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [MUL])
tr ls ctxt (Div e1 e2)   = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [DIV])
tr ls ctxt (Lt e1 e2)    = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [CGT])
tr ls ctxt (Lte e1 e2)   = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [CGTE])
tr ls ctxt (Gt e1 e2)    = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [CGT])
tr ls ctxt (Gte e1 e2)   = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [CGTE])
tr ls ctxt (Eq e1 e2)    = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [CEQ])
tr ls ctxt (Cons e1 e2)  = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [CONS])
tr ls ctxt (Car e)       = (tr ls ctxt e) <+> (toCode [CAR])
tr ls ctxt (Cdr e)       = (tr ls ctxt e) <+> (toCode [CDR])
tr ls ctxt (Nil e)       = (tr ls ctxt e) <+> (toCode [ATOM])
tr ls ctxt (App ef exps) = foldr (<+>) ((tr (split 0 ls) ctxt ef) <+> toCode [AP (length exps)]) 
                                      (zipWith (\e i->tr (split i ls) ctxt e) exps [1..])
tr (lt:lf:ls) ctxt (If e et ef) = (tr (split 0 ls) ctxt e) 
                             <+> (toCode [SEL lt lf])
                             <+> (floatAt lt ((tr (split 1 ls) ctxt et) <+> (toCode [JOIN])))
                             <+> (floatAt lf ((tr (split 2 ls) ctxt ef) <+> (toCode [JOIN])))
tr (l:ls) (ctxtm,lvl) (Lam vars exp) = let newcontext = ((M.fromList [(var, (lvl+1,i,[]))|  (var,i) <- zip vars [0..]]) `M.union` ctxtm,lvl+1) in
                                          (toCode [LDF l]) 
                                      <+> (floatAt l ( (tr ls newcontext exp)<+>(toCode [RTN])))
tr (l1:l2:ls) (ctxtm,lvl) (LamFApp f vars exp exps) = 
                   let l=length vars in
                   let newcontext = ((M.fromList [(var, (lvl+2,i,[]))| (var,i)<- zip vars [0..]]) `M.union` (M.fromList[(f, (lvl+1,0,[]))]) `M.union` ctxtm, lvl+2) in
                    case length exps == l of
                     True -> (toCode [DUM 1, LDF l1, LDF l2, RAP 1]
                              <+> (floatAt l1 ((tr (split 0 ls) newcontext exp)<+>(toCode [RTN])))
                              <+> (floatAt l2 ((foldr1 (<+>) (zipWith (\e i->tr (split i ls) (ctxtm,lvl+1) e) exps [1..]))<+>(toCode [LD 0 0, AP l, RTN]))))
                     False -> error "wrong number of arguments in LamFApp"
tr _ _ (IntLit n) = toCode [LDC n]
tr _ (ctxtm,lvl) (Name str) = case M.lookup str ctxtm of
                               Nothing -> error ("unbound variable "++ str)
                               Just (l,i,ls) -> toCode ([LD (lvl-l) i]++ls)
tr _ _ (List [])            = toCode [LDC 0]
tr ls ctxt (List exps)      = foldr1 (<+>) ((zipWith (\e i->tr (split i ls) ctxt e) (exps++[IntLit 0]) [0..])
                                         ++(replicate (length exps) (toCode[CONS])))
tr _ _ (LamF _ _ _) = error "LamF in tr!"



resolveLabels :: LabelLCode -> [LInstr Int]
resolveLabels = (\(_,subs,code) -> map (resolve (M.fromList subs)) code) . (foldl f (0,[],[])) . (M.toAscList) 
                 where
                  f (n,subs,code) (str,instrs) = (n+(length instrs), (str,n):subs, code++instrs)
                  resolve subsm (SEL s1 s2) = case (M.lookup s1 subsm, M.lookup s2 subsm) of
                                                (Just t,Just f) -> (SEL t f)
                                                _               -> error "undefined label"
                  resolve subsm (LDF s)     = case (M.lookup s subsm) of
                                                (Just n)        -> (LDF n)
                                                _               -> error "undefined label"
                  resolve subsm x           = fmap (const 0) x


