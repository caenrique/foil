module Foil (foil, genSetRep) where
import Datos
import Datos.Metodos
import Control.Monad
import Math.Combinat.Sets
import Data.List

foil :: BC -> [Variable] -> Literal -> [Ejemplo] -> [Ejemplo] -> [Rule] -> [Rule]
foil _   _     _   _  [] r = r
foil bc const obj en ep r = foil bc const obj en newEp (r ++ [newRule])
    where newRule   = genRule bc bcEj const en ep (R obj [])
          newEp     = filterEj bc bcEj const newRule ep
          bcEj      = map (L (getName obj)) ep

genRule :: BC -> BC -> [Variable] -> [Ejemplo] -> [Ejemplo] -> Rule -> Rule
genRule _  _    _     [] _  r = r
genRule bc bcEj const en ep r@(R h ls)
    | ls == []  = genRule bc bcEj const newEn ep (R h [nextLit])
    | otherwise = genRule bc bcEj const newEn ep (R h (ls ++ [nextLit2]))
    where nextLit   = bestLiteral bc bcEj const ep en r $ genLiterals bc h $ hFreeVars r
          nextLit2  = bestLiteral bc bcEj const ep en r $ genLiterals (h:bc) h $ hFreeVars r
          newEn     = filter (cubre bc bcEj const (R h (nextLit:ls))) en

genLiterals :: BC -> Literal -> [Variable] -> [Literal]
genLiterals bc lobj vas = genLiterals' vas ++
    (concat $ map (\lit@(L n vs) ->
        filter (/=lobj) $
        map (\lstv ->
            foldr (\(v, nv) acc ->
                literalApply v nv acc) 
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

bestLiteral :: BC -> BC -> [Variable] -> [Ejemplo] -> [Ejemplo] -> Rule -> [Literal] -> Literal
bestLiteral bc bcEj const ejs ejsn r@(R h lts) ls =
    ls !! (index $ elemIndex (maximum gainval) gainval)
    where
        gainval = map (gain bc bcEj const ejs ejsn r) ls
        index (Just i)  = i
        index _         = 0

gain :: BC -> BC -> [Variable] -> [Ejemplo] -> [Ejemplo] -> Rule -> Literal -> Float
gain bc bcEj const ejs ejsn r@(R h lts) l =
    (t l) * ((log2 (p (r' l) `sDiv` (p (r' l) + n (r' l)))) - (log2 (p r `sDiv` (p r + n r))))
    where   p rd        = fromIntegral $ length . filter (cubre bc bcEj const rd) $ ejs
            n rd        = fromIntegral $ length . filter (cubre bc bcEj const rd) $ ejsn
            t lit       = p $ r' lit
            r' lit      = R h (lts ++ [l])
            log2 0      = 0
            log2 x      = logBase 2 x
            sDiv a 0    = a
            sDiv a b    = a / b

filterEj :: BC -> BC -> [Variable] -> Rule -> [Ejemplo] -> [Ejemplo]
filterEj bc bcEj const rule ep = filter (not . cubre bc bcEj const rule) ep

cubre :: BC -> BC -> [Variable] -> Rule -> Ejemplo -> Bool
cubre bc bcEj const r ej = or . map (evalRule bc bcEj r const) $ br
    where br = buildRule r const ej

buildRule :: Rule -> [Variable] -> Ejemplo -> [Rule]
buildRule r const ej = filtra $ posibleRules const . ruleApply r . zip (hFreeVars r) $ ej
    where filtra            = filter (not . hayBucle)
          hayBucle (R h l)  = h `elem` l

evalRule :: BC -> BC -> Rule -> [Variable] -> Rule -> Bool
evalRule bc bcEj r const (R h []) = True
evalRule bc bcEj r const rule@(R h (t:ts))
    | h `litEq` t   = (evalLit bcEj t) && (evalRule bc bcEj r const (R h ts))
    | otherwise     = (evalLit bc t) && (evalRule bc bcEj r const (R h ts))

posibleRules :: [Variable] -> Rule -> [Rule]
posibleRules const r = map (\a -> ruleApply r $ zip vars a) $ genSetRep (length vars) const
    where vars = bFreeVars r

genSetRep :: Int -> [a] -> [[a]]
genSetRep n vs = replicateM n vs 

genSetNoRep :: Eq a => Int -> [a] -> [[a]]
genSetNoRep 0 _ = [[]]
genSetNoRep n cs  = concat [map (x:) (genSetNoRep (n-1) $ delete x cs) | x <- cs]

ruleApply :: Rule -> [(Variable, Variable)] -> Rule
ruleApply (R hls ls) sust = R (head $ appl [hls] sust) $ appl ls sust
    where appl vars s = foldr (\(var, val) acc -> map (literalApply var val) acc) vars s

literalApply :: Variable -> Variable -> Literal -> Literal
literalApply var val l = setVars l $ map (\a -> if a==var then val else a) $ getVars l
