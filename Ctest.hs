module Ctest where
import Datos

bc = [ 
        L "padre" [Val "sharon", Val "bob"], 
        L "padre" [Val "bob", Val "victor"], 
        L "madre" [Val "bob", Val "marta"], 
        L "madre" [Val "victor", Val "ana"], 
        L "padre" [Val "tom", Val "bob"],
        L "mujer" [Val "marta"],
        L "mujer" [Val "ana"],
        L "mujer" [Val "sharon"]
     ]

ejs = [
        [Val "victor", Val "sharon"],
        [Val "marta", Val "sharon"]
      ]

--ejN = (filter (not . (`elem` [ej])) . genValues Val (length $ ej)) constant
constant = [ "victor", "sharon", "bob", "tom", "marta", "ana"]
objetivo = L "nieta" [Var "x", Var "y"]
rule = R objetivo []
