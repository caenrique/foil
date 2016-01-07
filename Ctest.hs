module Ctest where
import Datos

bc = [ 
        L "padre" [Val "sharon", Val "bob"], 
        L "padre" [Val "bob", Val "victor"], 
        L "padre" [Val "tom", Val "bob"],

        L "mujer" [Val "sharon"]
     ]

ejs = [
        [Val "victor", Val "sharon"]
      ]

--ejN = (filter (not . (`elem` [ej])) . genVal (length $ ej)) constant
constant = [ "victor", "sharon", "bob", "tom"]
objetivo = L "nieta" [Var "X", Var "Y"]
rule = R objetivo []
