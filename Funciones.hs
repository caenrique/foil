module Funciones (genRule, genValues, filterEj) where
import Datos
import Data.List

genRule :: BC -> [String] -> [Ejemplo] -> [Ejemplo] -> Rule -> Rule
genRule dom const en ep robj@(R h ls)
    | length en == 0 = robj
    | otherwise         = genRule dom const newEn ep (R h (nextLiteral:ls))
    where nextLiteral   = bestLiteral dom const ep en robj $ genLiterals dom $ freeVars robj
          newEn         = filter (cubre dom const (R h (nextLiteral:ls))) en

genLiterals :: BC -> [Variable] -> [Literal]
genLiterals bc vas =
    concat $ map (\lit@(L n vs) ->
        map (\lstv ->
            foldr (\(v, nv) acc ->
                applySust v nv acc) 
            lit $ zip vs lstv)
        (genValues Var (length vs) newVars))
    $ nubBy (\(L na _) (L nb _) -> na == nb) bc
    where newVars = map show $ posibleVars vas

posibleVars :: [Variable] -> [Variable]
posibleVars vs = vs ++ map (Var . ('z':) . show) [0..length vs - 2]

bestLiteral :: BC -> [String] -> [Ejemplo] -> [Ejemplo] -> Rule -> [Literal] -> Literal
bestLiteral dom const ejs ejsn r@(R h lts) ls =
    ls !! (index $ elemIndex (maximum gainval) gainval)
    where
        p rd    = fromIntegral $ (length . filter (cubre dom const rd)) ejs
        n rd    = fromIntegral $ (length . filter (cubre dom const rd)) ejsn
        gain l  = (t l) * ((log2 (p (r' l) `sDiv` (p (r' l) + n (r' l))))
                        - (log2 (p r `sDiv` (p r + n r))))
        r' l    = (R h (l:lts))
        gainval = map gain ls
        t l     = p $ r' l
        index (Just i)  = i
        index _         = 0
        log2 0  = 0
        log2 x  = logBase 2 x

sDiv a b | b == 0 = a | otherwise = a / b

filterEj :: BC -> [String] -> Rule -> [Ejemplo] -> [Ejemplo]
filterEj bc const rule ep = filter (not . cubre bc const rule) ep

cubre :: BC -> [String] -> Rule -> Ejemplo -> Bool
cubre bc const r ej = (or . map (evalRule bc)) (buildRule r const ej)

buildRule :: Rule -> [String] -> Ejemplo -> [Rule]
buildRule r const ej = (posibleRules const . apply r . zip (freeVars r)) ej

evalRule :: BC -> Rule -> Bool
evalRule bc (R _ t) = (and . map (`elem` bc)) t

posibleRules :: [String] -> Rule -> [Rule]
posibleRules const r =
    map (\a -> apply r $ zip vars a) $ genValues Val (length vars) const
    where vars = freeVars r

genValues :: (String -> Variable) -> Int -> [String] -> [[Variable]]
genValues fv 0 cs  = [[]]
genValues fv n cs  = concat [map ((fv x):) (genValues fv (n-1) cs) | x <- cs]

apply :: Rule -> [(Variable, Variable)] -> Rule
apply (R hls ls) sust = R (head $ appl [hls] sust) (appl ls sust)
    where appl vars s = foldr (\(var, val) acc -> map (applySust var val) acc) vars s

applySust :: Variable -> Variable -> Literal -> Literal
applySust var val (L n vs) = L n (map (\a -> if a==var then val else a) vs)

freeVars :: Rule -> [Variable]
freeVars (R h body) =
    (nub . foldr (\(L _ vs) acc -> (filter isVar vs) ++ acc) []) (h:body)

isVar :: Variable -> Bool
isVar (Var _)   = True
isVar _         = False

pretty :: [Rule] -> String
pretty (r:[]) = show r
pretty (r:rs) = show r ++ "\n" ++ pretty rs

