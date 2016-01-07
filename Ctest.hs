module Ctest where
import Datos

bc = [ 
        L "padre" [Val "sharon", Val "bob"], 
        L "padre" [Val "bob", Val "victor"], 
        L "padre" [Val "pepa", Val "antonio"], 
        L "padre" [Val "tom", Val "bob"],

        L "madre" [Val "tom", Val "pepa"],
        L "madre" [Val "bob", Val "marta"], 
        L "madre" [Val "antonio", Val "ana"], 
        L "madre" [Val "sharon", Val "pepa"], 

        L "mujer" [Val "marta"],
        L "mujer" [Val "ana"],
        L "mujer" [Val "pepa"],
        L "mujer" [Val "sharon"]
     ]

ejs = [
        [Val "victor", Val "sharon"],
        [Val "ana", Val "pepa"],
        [Val "antonio", Val "sharon"],
        [Val "marta", Val "sharon"]
      ]

--ejN = (filter (not . (`elem` [ej])) . genValues Val (length $ ej)) constant
constant = [ "victor", "sharon", "bob", "tom", "marta", "ana", "antonio", "pepa"]
objetivo = L "nieta" [Var "X", Var "Y"]
rule = R objetivo []
