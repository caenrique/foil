-- | Implementación de la funcionalidad principal del algoritmo foil
module Foil where
import Datos
import Datos.Metodos
import Control.Monad
import Math.Combinat.Sets
import Data.List

-- | A partir de conocimiento de fondo y ejemplos, la función 'foil' extrae reglas 
-- para describir un enunciado mediante lógica de primer orden
foil :: BC              -- ^ Base de conocimiento.
     -> [Variable]      -- ^ Lista de variables del dominio. 
     -> Literal         -- ^ Literal que representa al objetivo. 
     -> [Ejemplo]       -- ^ Lista de ejemplos negativos. 
     -> [Ejemplo]       -- ^ Lista de ejemplos positivos. 
     -> [Rule]          -- ^ Lista de reglas. Utilizado por la función recursiva 
                        --   para ir guardando las reglas que va calculando. 
     -> [Rule]          -- ^ Lista de reglas extraidas.
foil _  _     _   _  [] r = r
foil bc dom obj en ep r = foil bc dom obj en newEp (r ++ [newRule])
    where newRule   = genRule bc bcEj dom en ep (R obj [])
          newEp     = filterEj bc bcEj dom newRule ep
          bcEj      = map (L (getName obj)) ep

-- | La función 'genRule' genera una regla mediante un proceso recursivo, 
-- en el cual va añadiendole restricciones hasta que no cubre ningún ejemplo negativo.
genRule :: BC           -- ^ Base de conocimiento.
        -> BC           -- ^ Base de conocimiento solo con los ejemplos. 
        -> [Variable]   -- ^ lista de variables del dominio.
        -> [Ejemplo]    -- ^ Lista de ejemplos negativos.
        -> [Ejemplo]    -- ^ Lista de ejemplos positivos.
        -> Rule         -- ^ Regla que se va actualizando en cada llamada recursiva.
        -> Rule
genRule _  _    _     [] _  r = r
genRule bc bcEj dom en ep r@(R h ls)
    | ls == []  = genRule bc bcEj dom newEn ep (R h [nextLit])
    | otherwise = genRule bc bcEj dom newEn ep (R h (ls ++ [nextLit2]))
    where nextLit   = bestLiteral bc bcEj dom ep en r $ genLiterals bc h $ hFreeVars r
          nextLit2  = bestLiteral bc bcEj dom ep en r $ genLiterals (h:bc) h $ hFreeVars r
          newEn     = filter (cubre bc bcEj dom (R h (nextLit:ls))) en

-- | La función 'genLiterals' genera todos los posibles literales 
-- a partir de un literal dado y una serie de Variables
genLiterals :: BC -> Literal -> [Variable] -> [Literal]
genLiterals bc lobj vas = genLiterals' vas ++
    (concat $ map (\lit@(L _ vs) ->
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

-- | La función 'genLiterals'' es una función auxiliar a  'genLiterals'. 
-- Se encarga de generar los literales formados por == y \\=.
genLiterals' :: [Variable] -> [Literal]
genLiterals' vas = concat $ map (\(a0:a1:[]) -> [E a0 a1, NE a0 a1]) $ choose 2 vas 

-- | La función 'bestLiteral calcula cuál es el mejor literal 
-- de los proporcionados, usando la función 'gain' para calcular
-- la ganancia de cada uno de ellos.
bestLiteral :: BC           -- ^ Base de conocimiento.
            -> BC           -- ^ Base de conocimiento solo con los ejemplos. 
            -> [Variable]   -- ^ Lista de variables del dominio.
            -> [Ejemplo]    -- ^ Lista de ejemplos positivos.
            -> [Ejemplo]    -- ^ Lista de ejemplos negativos.
            -> Rule         -- ^ Regla a la que añadirle el literal.
            -> [Literal]    -- ^ Lista de literales a evaluar.
            -> Literal
bestLiteral bc bcEj dom ejs ejsn r ls =
    ls !! (index $ elemIndex (maximum gainval) gainval)
    where
        gainval = map (gain bc bcEj dom ejs ejsn r) ls
        index (Just i)  = i
        index _         = 0

-- | La función 'gain' calcula la ganacia de un literal al incluirlo en una regla 
-- mediante la fórmula: t * ( log2(pr' / (pr' + nr')) - log2(pr / (pr + nr)) )
gain :: BC              -- ^ Base de conocimiento.
     -> BC              -- ^ Base de conocimiento solo con los ejemplos.
     -> [Variable]      -- ^ Lista de variables del dominio.
     -> [Ejemplo]       -- ^ Lista de ejemplos positivos. 
     -> [Ejemplo]       -- ^ Lista de ejemplos negativos.
     -> Rule            -- ^ Regla a la que añadirle el literal.
     -> Literal         -- ^ Literal sobre el que se calcula la ganancia.
     -> Float
gain bc bcEj dom ejs ejsn r@(R h lts) l =
    t * ((log2 (p r' `sDiv` (p r' + n r'))) - (log2 (p r `sDiv` (p r + n r))))
    where   p rd        = fromIntegral $ length . filter (cubre bc bcEj dom rd) $ ejs
            n rd        = fromIntegral $ length . filter (cubre bc bcEj dom rd) $ ejsn
            t           = p r'
            r'          = R h (lts ++ [l])
            log2 0      = 0
            log2 x      = logBase 2 x
            sDiv a 0    = a
            sDiv a b    = a / b

-- | La función 'cubre' determina si una regla cubre a determinado ejemplo o no.
cubre :: BC             -- ^ Base de conocimiento.
      -> BC             -- ^ Base de conocimiento solo con los ejemplos.
      -> [Variable]     -- ^ Lista de variables del dominio.
      -> Rule           -- ^ Regla a comprobar
      -> Ejemplo        -- ^ Ejemplo con el que comprobar la regla
      -> Bool
cubre bc bcEj dom r ej = or . map (evalRule bc bcEj r dom) $ br
    where br = buildRule r dom ej

-- | La función 'buildRule' toma una regla con variables libre y un ejemplo
-- y construye las posibles reglas.
buildRule :: Rule -> [Variable] -> Ejemplo -> [Rule]
buildRule r dom ej = filtra $ posibleRules dom . ruleApply r . zip (hFreeVars r) $ ej
    where filtra            = filter (not . hayBucle)
          hayBucle (R h l)  = h `elem` l

-- | La función 'evalRule' evalúa si una regla es cierta o no.
evalRule :: BC -> BC -> Rule -> [Variable] -> Rule -> Bool
evalRule _  _    _ _   (R _ []) = True
evalRule bc bcEj r dom (R h (t:ts))
    | h `litEq` t   = (evalLit bcEj t) && (evalRule bc bcEj r dom (R h ts))
    | otherwise     = (evalLit bc t) && (evalRule bc bcEj r dom (R h ts))

-- | La función 'posibleRules' calcula todas las posibles reglas basadas en la regla 
-- proporcionada aplicando a las variables libres valores del dominio.
posibleRules :: [Variable] -> Rule -> [Rule]
posibleRules dom r = map (\a -> ruleApply r $ zip vars a) $ genSetRep (length vars) dom
    where vars = bFreeVars r

-- | La función 'posibleVars' devuelve las variables permitidas en base a las 
-- variables que le pasan por parámetro. Las variables permitidas son n + (n-1), 
-- donde n es el número de variables de partida.
posibleVars :: [Variable] -> [Variable]
posibleVars vs = vs ++ map (Var . ('Z':) . show) [0..length vs - 2]

-- | La función 'ruleApply' toma una regla y una lista de pares de variables, 
-- y aplica las sustituciones de la primera variable del par por la segunda
-- en dicha regla.
ruleApply :: Rule -> [(Variable, Variable)] -> Rule
ruleApply (R hls ls) sust = R (head $ appl [hls] sust) $ appl ls sust
    where appl vars s = foldr (\(var, val) acc -> map (literalApply var val) acc) vars s

-- | La función 'literalApply' toma dos variables y un literal y sustituye todas
-- las ocurrencias de la primera variable por la segunda en dicho literal.
literalApply :: Variable -> Variable -> Literal -> Literal
literalApply var val l = setVars l $ map (\a -> if a==var then val else a) $ getVars l

-- | La función 'filterEj' filtra los ejemplos cubiertos por la regla dada,
-- devolviendo así solo aquellos ejemplos no cubiertos por dicha regla.
filterEj :: BC -> BC -> [Variable] -> Rule -> [Ejemplo] -> [Ejemplo]
filterEj bc bcEj dom rule ep = filter (not . cubre bc bcEj dom rule) ep

-- | La función 'genSetRep' genera todas las posibles combinaciones de tamaño 'n'
-- de los elementos en 'vs', incluyendo combinaciones con elementos repetidos.
genSetRep :: Int -> [a] -> [[a]]
genSetRep n vs = replicateM n vs

-- | La función 'genSetNoRep' genera todas las posibles combinaciones de tamaño 'n'
-- de los elementos en 'cs'. Dos combinaciones con los mismo elementos pero en distinto orden
-- son diferentes.
genSetNoRep :: Eq a => Int -> [a] -> [[a]]
genSetNoRep 0 _ = [[]]
genSetNoRep n cs  = concat [map (x:) (genSetNoRep (n-1) $ delete x cs) | x <- cs]
