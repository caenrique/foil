-- | Módulo que contiene diferentes métodos para trabajar con los datos definidos en 
-- el módulo 'Datos'.
module Datos.Metodos where
import Data.List
import Datos

-- | La función 'isVar' determina si una 'Variable' se creo con el contructor 'Var'.
isVar :: Variable -> Bool
isVar (Var _)   = True
isVar _         = False

-- | La función 'prettyPrint' convierte una lista de reglas ('Rule') en un string separado 
-- por saltos de línea.
prettyPrint :: [Rule] -> String
prettyPrint [] = ""
prettyPrint (r:rs) = show r ++ "\n" ++ prettyPrint rs

-- | La función 'getName' extrae el nombre de un 'Literal'.
getName :: Literal -> String
getName (L nombre _)    = nombre
getName _               = []

-- | La función 'getVars' extrae las 'Variable's de un 'Literal'.
getVars :: Literal -> [Variable]
getVars (L _ vars) = vars
getVars (E a b) = [a,b]
getVars (NE a b) = [a,b]

-- | La fucnión 'setVars' devuelve un 'Literal' con la lista de 'Variable's pasada.
setVars :: Literal -> [Variable] -> Literal
setVars (L n _) v = L n v
setVars (E _ _) (v0:v1:_) = E v0 v1
setVars (NE _ _) (v0:v1:_) = NE v0 v1
setVars x _ = x

-- | La función 'getConstants' extrae todas las diferentes constantes de un 'BC'.
getConstants :: BC -> [Variable]
getConstants bc = nub . concat . map getVars $ bc

-- | La función 'litEq' compara dos 'Literal'es solo por el nombre.
litEq :: Literal -> Literal -> Bool
litEq (L n _) (L m _)   = n == m
litEq _       _         = False

-- | La función 'evalLit' determina si un 'Literal' es cierto o no.
evalLit :: BC -> Literal -> Bool
evalLit _  (E a b)  = a == b
evalLit _  (NE a b) = a /= b
evalLit bc l        = l `elem` bc

-- | La fucnión 'hFreeVars' extrae las 'Variable's libres de la cabeza de una regla ('Rule'). 
hFreeVars :: Rule -> [Variable]
hFreeVars (R (L _ vs) _)    = filter isVar vs
hFreeVars (R (E a b) _)     = filter isVar [a,b]
hFreeVars (R (NE a b) _)    = filter isVar [a,b]

-- | La función 'bFreeVars' extrae las 'Variable's libres del cuerpo de una regla ('Rule').
bFreeVars :: Rule -> [Variable]
bFreeVars (R _ body) = nub . foldr lit [] $ body
    where  lit (L _ vs) acc = (filter isVar vs) ++ acc
           lit (E a b) acc  = (filter isVar [a,b]) ++ acc
           lit (NE a b) acc = (filter isVar [a,b]) ++ acc
