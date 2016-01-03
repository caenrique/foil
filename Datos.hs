module Datos where
import Data.List

type BC = [Literal]
type Ejemplo = [Variable]

data Variable   = Var String | Val String deriving(Eq)
data Rule       = R Literal [Literal]
data Literal    = L String [Variable] deriving(Eq)

instance Show Variable where
    show (Var a) = a
    show (Val a) = a

instance Show Rule where
    show (R head tail) = shows head $ ':':'-':' ':(intercalate ", " (map show tail)) ++ "."

instance Show Literal where
    show (L name params) = name ++ '(':(intercalate "," $ map show params) ++ ")"
