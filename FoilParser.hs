module FoilParser (parseFoil, parseObjetivo) where

import Text.ParserCombinators.Parsec
import Control.Applicative (liftA2)
import Datos

foilFile :: CharParser () [Literal]
foilFile = endBy literal (many eol)

literal :: CharParser () Literal
literal = liftA2 L (name) (char '(' *> sepBy variable (char ',' *> many (char ' ')) <* char ')')

objetivo :: CharParser () Literal
objetivo = liftA2 L (name) (char '(' *> sepBy var (char ',' *> many (char ' ')) <* char ')')

name :: CharParser () String
name = many (noneOf ",\n()")

variable :: CharParser () Variable
variable = Val <$> many (noneOf ",\n()")

var :: CharParser () Variable
var = Var <$> many (noneOf ",\n()")

eol :: CharParser () Char
eol = char '\n'

parseFoil :: String -> Either ParseError [Literal]
parseFoil input = parse foilFile "(unknown)" input

parseObjetivo :: String -> Either ParseError Literal
parseObjetivo input = parse objetivo "(unknown)" input
