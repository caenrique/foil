import System.Environment
import Funciones
import Datos
import Ctest

foil :: BC -> [String] -> Literal -> [Ejemplo] -> [Ejemplo] -> [Rule] -> [Rule]
foil _   _     _   _  [] r = r
foil dom const obj en ep r = foil dom const obj en newEp (r ++ [newRule])
    where newRule   = genRule dom const en ep (R obj [])
          newEp     = filterEj dom const newRule ep

parseLiteral :: String -> Literal
parseLiteral = undefined

getConstantes :: [Literal] -> [String]
getConstantes = undefined

main :: IO ()
main = do
    [bcfilename, ejfilename, objetivoStr] <- getArgs
    bcfilestring <- readFile bcfilename
    ejfilestring <- readFile ejfilename
    let ejemplos = map ((map Val) . words) $ lines ejfilestring
    let bc = map parseLiteral $ lines bcfilestring
    let objetivo = parseLiteral objetivoStr
    let const = getConstantes bc
    let ejN = (filter (not . (`elem` ejemplos)) . genVal (length $ head ejemplos)) const
    putStrLn $ show $ foil bc const objetivo ejN ejemplos []


