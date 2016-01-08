import System.Environment
import Funciones
import Datos
import FoilParser

foil :: BC -> [String] -> Literal -> [Ejemplo] -> [Ejemplo] -> [Rule] -> [Rule]
foil _   _     _   _  [] r = r
foil dom const obj en ep r = foil dom const obj en newEp (r ++ [newRule])
    where newRule   = genRule dom const en ep (R obj [])
          newEp     = filterEj dom const newRule ep

getFromEither :: Either a b -> b
getFromEither (Right x) = x
getFromEither (Left _)  = error "error al parsear"

main :: IO ()
main = do
    [filename, obj] <- getArgs
    fileString <- readFile filename
    let parseado = getFromEither $ parseFoil fileString
    let lobj = getFromEither $ parseObjetivo obj
    let objName = getName lobj
    let bc = filter ((/=objName) . getName) parseado
    let ejemplos = map getVars $ filter ((==objName) . getName) parseado
    let const = getConstants bc
    let ejN = filter (not . (`elem` ejemplos)) . genVal (length $ getVars lobj) $ const
    putStrLn $ pretty $ foil bc const lobj ejN ejemplos []


