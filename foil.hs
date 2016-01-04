import System.Environment
import Funciones
import Datos

foil :: [Ejemplo] -> BC -> [String] -> Literal -> [Rule] -> [Rule]
foil [] _   _     _   r = r
foil ep dom const obj r = foil newEp dom const obj (newRule:r)
    where newRule   = genRule dom const ep (R obj [])
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
    putStrLn $ show $ foil ejemplos bc (getConstantes bc) objetivo []


