module Main where
import System.Environment
import Foil
import Datos.Metodos
import FoilParser

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
    let dom = getConstants bc
    let ejN = filter (not . (`elem` ejemplos)) . genSetRep (length $ getVars lobj) $ dom
    putStrLn $ prettyPrint $ foil bc dom lobj ejN ejemplos []


