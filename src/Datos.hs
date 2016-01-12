module Datos where
import Data.List

type BC = [Literal]
type Ejemplo = [Variable]

data Variable   = Var String | Val String deriving(Eq)
data Rule       = R Literal [Literal]
data Literal    = L String [Variable]
                | E Variable Variable
                | NE Variable Variable
                deriving(Eq)

instance Show Variable where
    show (Var a) = a
    show (Val a) = a

instance Show Rule where
    show (R h t) = shows h $ ':':'-':' ':(intercalate ", " (map show t)) ++ "."

instance Show Literal where
    show (L name params) = name ++ '(':(intercalate "," $ map show params) ++ ")"
    show (E x1 x2) = show x1 ++ "==" ++ show x2
    show (NE x1 x2) = show x1 ++ "\\=" ++ show x2
