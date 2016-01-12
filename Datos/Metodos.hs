module Datos.Metodos where
import Data.List
import Datos

isVar :: Variable -> Bool
isVar (Var _)   = True
isVar _         = False

prettyPrint :: [Rule] -> String
prettyPrint [] = ""
prettyPrint (r:rs) = show r ++ "\n" ++ prettyPrint rs

getName :: Literal -> String
getName (L nombre _) = nombre

getVars :: Literal -> [Variable]
getVars (L _ vars) = vars
getVars (E a b) = [a,b]
getVars (NE a b) = [a,b]

setVars :: Literal -> [Variable] -> Literal
setVars (L n _) v = L n v
setVars (E _ _) (v0:v1:vs) = E v0 v1
setVars (NE _ _) (v0:v1:vs) = NE v0 v1
setVars x _ = x

getConstants :: BC -> [Variable]
getConstants bc = nub . concat . map getVars $ bc

litEq :: Literal -> Literal -> Bool
litEq (L n _) (L m _)   = n == m
litEq _       _         = False


evalLit :: BC -> Literal -> Bool
evalLit _  (E a b)      = a == b
evalLit _  (NE a b)     = a /= b
evalLit bc l            = l `elem` bc

hFreeVars :: Rule -> [Variable]
hFreeVars (R (L _ vs) _) = filter isVar vs

bFreeVars :: Rule -> [Variable]
bFreeVars (R _ body) = nub . foldr lit [] $ body
    where   lit (L _ vs) acc = (filter isVar vs) ++ acc
            lit (E a b) acc = (filter isVar [a,b]) ++ acc
            lit (NE a b) acc = (filter isVar [a,b]) ++ acc
