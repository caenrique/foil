module FoilParser (parseFoil, parseObjetivo) where

import Text.ParserCombinators.Parsec
import Control.Applicative (liftA2)
import Datos

lexema :: Parser a -> Parser a
lexema p = spaces *> p <* spaces

foilFile = endBy literal (many eol)
objetivo = liftA2 L (name) (char '(' *> sepBy var (separator) <* char ')' <* spaces)
literal = liftA2 L (name) (char '(' *> sepBy variable (separator) <* char ')' <* spaces)
name = lexema $ many (noneOf ",\n() ") <* spaces
variable = lexema $ Val <$> many (noneOf ",\n() ")
var = lexema $ Var <$> many (noneOf ",\n() ")
eol = char '\n'
separator = lexema $ char ','

parseFoil :: String -> Either ParseError [Literal]
parseFoil input = parse foilFile "(unknown)" input

parseObjetivo :: String -> Either ParseError Literal
parseObjetivo input = parse objetivo "(unknown)" input
