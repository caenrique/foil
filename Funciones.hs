module Funciones (pretty, getName, getVars, getConstants, genRule, filterEj, genSetRep) where
import Datos
import Control.Monad
import Math.Combinat.Sets
import Data.List

genRule :: BC -> [Variable] -> [Ejemplo] -> [Ejemplo] -> Rule -> Rule
genRule dom const en ep robj@(R h ls)
    | length en == 0 = robj
    | otherwise         = genRule dom const newEn ep (R h (ls ++ [nextLiteral]))
    where nextLiteral   = bestLiteral dom const ep en robj $ genLiterals dom $ freeVarsHead robj
          newEn         = filter (cubre dom const (R h (nextLiteral:ls))) en

genLiterals :: BC -> [Variable] -> [Literal]
genLiterals bc vas = genLiterals' vas ++
    (concat $ map (\lit@(L n vs) ->
        map (\lstv ->
            foldr (\(v, nv) acc ->
                applySust v nv acc) 
            lit $ zip vs lstv)
        $ ifOneVar (length vs))
    $ nubBy (\(L na _) (L nb _) -> na == nb) bc)
    where   newVars     = posibleVars vas
            ifOneVar 1  = map (:[]) vas
            ifOneVar n  = genSetNoRep n newVars

genLiterals' :: [Variable] -> [Literal]
genLiterals' vas = concat $ map (\(a0:a1:[]) -> [E a0 a1, NE a0 a1]) $ choose 2 vas 

posibleVars :: [Variable] -> [Variable]
posibleVars vs = vs ++ map (Var . ('Z':) . show) [0..length vs - 2]

bestLiteral :: BC -> [Variable] -> [Ejemplo] -> [Ejemplo] -> Rule -> [Literal] -> Literal
bestLiteral dom const ejs ejsn r@(R h lts) ls =
    ls !! (index $ elemIndex (maximum gainval) gainval)
    where
        gainval = map (gain dom const ejs ejsn r) ls
        index (Just i)  = i
        index _         = 0

gain :: BC -> [Variable] -> [Ejemplo] -> [Ejemplo] -> Rule -> Literal -> Float
gain dom const ejs ejsn r@(R h lts) l =
    (t l) * ((log2 (p (r' l) `sDiv` (p (r' l) + n (r' l)))) - (log2 (p r `sDiv` (p r + n r))))
    where   p rd        = fromIntegral $ length . filter (cubre dom const rd) $ ejs
            n rd        = fromIntegral $ length . filter (cubre dom const rd) $ ejsn
            t lit       = p $ r' lit
            r' lit      = R h (l:lts)
            log2 0      = 0
            log2 x      = logBase 2 x
            sDiv a 0    = a
            sDiv a b    = a / b

filterEj :: BC -> [Variable] -> Rule -> [Ejemplo] -> [Ejemplo]
filterEj bc const rule ep = filter (not . cubre bc const rule) ep

cubre :: BC -> [Variable] -> Rule -> Ejemplo -> Bool
cubre bc const r ej = or . map (evalRule bc) $ buildRule r const ej

buildRule :: Rule -> [Variable] -> Ejemplo -> [Rule]
buildRule r const ej = posibleRules const . apply r . zip (freeVarsHead r) $ ej

evalRule :: BC -> Rule -> Bool
evalRule bc (R _ t) = and . map evalLit $ t
    where   evalLit l@(L _ _) = l `elem` bc
            evalLit (E a b) = a == b
            evalLit (NE a b) = a /= b

posibleRules :: [Variable] -> Rule -> [Rule]
posibleRules const r =
    map (\a -> apply r $ zip vars a) $ genSetRep (length vars) const
    where vars = freeVarsBody r

genSetRep :: Int -> [Variable] -> [[Variable]]
genSetRep n vs = replicateM n vs 

genSetNoRep :: Int -> [Variable] -> [[Variable]]
genSetNoRep 0 _ = [[]]
genSetNoRep n cs  = concat [map (x:) (genSetNoRep (n-1) $ delete x cs) | x <- cs]

apply :: Rule -> [(Variable, Variable)] -> Rule
apply (R hls ls) sust = R (head $ appl [hls] sust) $ appl ls sust
    where appl vars s = foldr (\(var, val) acc -> map (applySust var val) acc) vars s

applySust :: Variable -> Variable -> Literal -> Literal
applySust var val l = setVars l $ map (\a -> if a==var then val else a) $ getVars l

freeVarsHead :: Rule -> [Variable]
freeVarsHead (R (L _ vs) _) = filter isVar vs

freeVarsBody :: Rule -> [Variable]
freeVarsBody (R _ body) = nub . foldr lit [] $ body
    where   lit (L _ vs) acc = (filter isVar vs) ++ acc
            lit (E a b) acc = (filter isVar [a,b]) ++ acc
            lit (NE a b) acc = (filter isVar [a,b]) ++ acc

isVar :: Variable -> Bool
isVar (Var _)   = True
isVar _         = False

pretty :: [Rule] -> String
pretty (r:[]) = show r
pretty (r:rs) = show r ++ "\n" ++ pretty rs

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
