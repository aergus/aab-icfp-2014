{-# LANGUAGE DeriveFunctor #-}
module Lisp where

import qualified Data.Map as M


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
            | List [Expression]
            | LamFApp String [String] Expression [Expression]  --only for compilation

prepareRecursions :: 

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
           
type Context = (M.Map String (Int,Int),Int) --how deep is each variable, and how deep are we?
newcontext :: Context
newcontext = (M.fromList [("worldstate",(0,0)),("undocumented",(0,1))],0) --the two arguments of main

tr :: [Label] -> Context -> Expression -> LabelLCode
tr ls ctxt (Add e1 e2)   = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [ADD])
tr ls ctxt (Sub e1 e2)   = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [SUB])
tr ls ctxt (Mul e1 e2)   = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [MUL])
tr ls ctxt (Div e1 e2)   = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [DIV])
tr ls ctxt (Lt e1 e2)    = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [CGT])
tr ls ctxt (Lte e1 e2)   = (tr (split 0 ls) ctxt e1) <+> (tr (split 1 ls) ctxt e2) <+> (toCode [CGTE])
tr ls ctxt (Gt e1 e2)    = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [CGT])
tr ls ctxt (Gte e1 e2)   = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [CGTE])
tr ls ctxt (Eq e1 e2)    = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [CEQ])
tr ls ctxt (Cons e1 e2)  = (tr (split 0 ls) ctxt e2) <+> (tr (split 1 ls) ctxt e1) <+> (toCode [CONS])
tr ls ctxt (Car e)       = (tr ls ctxt e) <+> (toCode [CAR])
tr ls ctxt (Cdr e)       = (tr ls ctxt e) <+> (toCode [CAR])
tr ls ctxt (App ef exps) = foldr (<+>) ((tr (split 0 ls) ctxt ef) <+> toCode [AP (length exps)]) 
                                      (zipWith (\e i->tr (split i ls) ctxt e) exps [1..])
tr (lt:lf:ls) ctxt (If e et ef) = (tr (split 0 ls) ctxt e) 
                             <+> (toCode [SEL lt lf])
                             <+> (floatAt lt ((tr (split 1 ls) ctxt et) <+> (toCode [JOIN])))
                             <+> (floatAt lf ((tr (split 2 ls) ctxt ef) <+> (toCode [JOIN])))
tr (l:ls) (ctxtm,lvl) (Lam vars exp) = let newcontext = ((M.fromList (zipWith (\var i ->(var, (lvl+1,i))) vars [0..])) `M.union` ctxtm,lvl+1) in
                                          (toCode [LDF l]) 
                                      <+> (floatAt l ( (tr ls newcontext exp)<+>(toCode [RTN])))
tr _ _ (IntLit n) = toCode [LDC n]
tr _ (ctxtm,lvl) (Name str) = case M.lookup str ctxtm of
                               Nothing -> error "unbound variable!"
                               Just (l,i) -> toCode [LD (lvl-l) i]
tr _ _ (List [])            = toCode [LDC 0]
tr ls ctxt (List exps)      = foldr1 (<+>) ((zipWith (\e i->tr (split i ls) ctxt e) exps [0..])++(replicate (length exps -1) (toCode [CONS])))

transform :: Expression -> LabelLCode
transform exp = tr rootlabels newcontext exp






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


