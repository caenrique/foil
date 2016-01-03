import Data.List
import Datos

foil :: [Ejemplo] -> BC -> [String] -> Literal -> [Rule] -> [Rule]
foil [] _   _     _   r = r
foil ep dom const obj r = foil newEp dom const obj (newRule:r)
    where newRule   = genRule ep en dom obj
          newEp     = filterEj dom const newRule ep
          en        = (filter (not . (`elem` ep)) . genValues (length $ head ep)) const

genRule :: [Ejemplo] -> [Ejemplo] -> BC -> Literal -> Rule
genRule ep en dom obj = undefined

filterEj :: BC -> [String] -> Rule -> [Ejemplo] -> [Ejemplo]
filterEj bc const rule ep = filter (cubre bc const rule) ep

cubre :: BC -> [String] -> Rule -> Ejemplo -> Bool
cubre bc const r ej = (or . map (evalRule bc)) (buildRule r const ej)

buildRule :: Rule -> [String] -> Ejemplo -> [Rule]
buildRule r const ej = (posibleRules const . apply r . zip (freeVars r)) ej

evalRule :: BC -> Rule -> Bool
evalRule bc (R _ t) = (and . map (`elem` bc)) t

posibleRules :: [String] -> Rule -> [Rule]
posibleRules const r =
    map (\a -> apply r $ zip vars a) $ genValues (length vars) const
    where vars = freeVars r

genValues :: Int -> [String] -> [[Variable]]
genValues 1 cs  = map (\a -> (Val a):[]) cs
genValues n cs  = concat [map ((Val x):) (genValues (n-1) (delete x cs)) | x <- cs]

apply :: Rule -> [(Variable, Variable)] -> Rule
apply (R hls ls) sust = R (head $ appl [hls] sust) (appl ls sust)
    where appl vars s = foldr (\(var, val) acc -> map (applySust var val) acc) vars s

applySust :: Variable -> Variable -> Literal -> Literal
applySust var val (L n vs) = L n (map (\a -> if a==var then val else a) vs)

freeVars :: Rule -> [Variable]
freeVars (R h body) =
    (nub . foldr (\(L _ vs) acc -> acc ++ (filter isVar vs)) []) (h:body)

isVar :: Variable -> Bool
isVar (Var _)   = True
isVar _         = False
