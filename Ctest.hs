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

--ejN = (filter (not . (`elem` ejs)) . genVal (length $ head ejs)) constant
constant = [ "victor", "sharon", "bob", "tom"]
objetivo = L "nieta" [Var "X", Var "Y"]
rule = R objetivo []
