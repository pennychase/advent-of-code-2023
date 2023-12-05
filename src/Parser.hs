module Parser where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

intParser :: Parser Int
intParser = lexeme L.decimal

signedInt :: Parser Integer
signedInt = L.signed hspace L.decimal

csvParser :: Parser a -> Parser [a]
csvParser p = do
    vals <- sepEndBy1 p (char ',') 
    pure vals

pairParser :: Char -> Parser (Int, Int)
pairParser c = do
    i <- intParser
    char c
    j <- intParser
    pure (i,j)

lineParser :: Parser a -> Parser [a]
lineParser parser = sepEndBy1 parser eol

readInput' :: Text -> Parser a -> IO a
readInput' str parser = do
    case runParser parser "" str of
        Left _ -> error "Unable to parse input"
        Right input -> pure input

readInput :: FilePath -> Parser a -> IO a
readInput path parser = do
    contents <- T.readFile path
    case runParser parser "" contents of
        Left _ -> error "Unable to parse input"
        Right input -> pure input
