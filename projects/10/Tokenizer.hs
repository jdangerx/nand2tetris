module Tokenizer where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)

data Terminal = Keyword Keyword
              | Symbol Symbol
              | IntCons IntCons
              | StringCons StringCons
              | Identifier Identifier
                deriving Show

data Keyword = Class | Constructor | Function | Method | Field | Static | Var
             | Int | Char | Boolean | Void | TrueKW | FalseKW | Null | This | Let
             | Do | If | Else | While | Return
             deriving (Show, Ord, Eq)

kwMap :: M.Map Keyword String
kwMap = M.fromList
  [ (Class, "class"), (Constructor, "constructor"), (Function, "function")
  , (Method, "method") , (Field, "field") , (Static, "static")
  , (Var, "var") , (Int, "int") , (Char, "char") , (Boolean, "boolean")
  , (Void, "void") , (TrueKW, "true") , (FalseKW, "false")
  , (Null, "null") , (This, "this") , (Let, "let") , (Do, "do")
  , (If, "if") , (Else, "else") , (While, "while") , (Return, "return") ]

data Symbol = OBrace | CBrace | OParen | CParen | OBracket | CBracket | Dot
            | Comma | Semi | Plus | Minus | Star | Slash | Amp | Pipe | Lt
            | Gt | Eq | Tilde
            deriving (Show, Ord, Eq)

symMap :: M.Map Symbol String
symMap = M.fromList
  [ (OBrace, "{") , (CBrace, "}") , (OParen, "(") , (CParen, ")")
  , (OBracket, "[") , (Dot, ".") , (Comma, ",") , (CBracket, "]") , (Semi, ";")
  , (Plus, "+") , (Minus, "-") , (Star, "*") , (Slash, "/") , (Amp, "&")
  , (Pipe, "|") , (Lt, "<") , (Gt, ">") , (Eq, "=") , (Tilde, "~") ]

type IntCons = Integer
type StringCons = String
type Identifier = String

class XMLWriter a where
  toXML :: a -> String

instance XMLWriter Keyword where
  toXML kw =
    let xmlSym s = fromMaybe "" . M.lookup s $ kwMap
    in "<keyword> " ++ xmlSym kw ++ " </keyword>"

instance XMLWriter Symbol where
  toXML sym =
    let xmlSym Lt = "&lt;"
        xmlSym Gt = "&gt;"
        xmlSym Amp = "&amp;"
        xmlSym s = fromMaybe "" . M.lookup s $ symMap
    in "<symbol> " ++ xmlSym sym ++ " </symbol>"

instance XMLWriter Terminal where
  toXML (Keyword kw) = toXML kw
  toXML (Symbol sym) = toXML sym
  toXML (IntCons intCons) =
    "<integerConstant> " ++ show intCons ++ " </integerConstant>"
  toXML (StringCons strCons) =
    "<stringConstant> " ++ strCons ++ " </stringConstant>"
  toXML (Identifier ident) =
    "<identifier> " ++ ident ++ " </identifier>"

jackDef :: P.LanguageDef st
jackDef = javaStyle
          { P.identStart = letter <|> char '_'
          , P.identLetter = alphaNum <|> char '_'
          , P.reservedNames = M.elems kwMap
          , P.reservedOpNames = M.elems symMap }

tok :: P.TokenParser ()
tok = P.makeTokenParser jackDef

symbol :: String -> Parser String
symbol = P.symbol tok

natural :: Parser Integer
natural = P.natural tok

stringLiteral :: Parser String
stringLiteral = P.stringLiteral tok

identifier :: Parser String
identifier = P.identifier tok

comment :: Parser ()
comment =
  try ((string "/*" <|> string "/**") >> manyTill anyChar (try $ string "*/") >> return ())
  <|> try (string "//" >> (many . noneOf $ "\n\r") >> return ())

parseKeyword :: Parser Keyword
parseKeyword =
  foldr (\(kw, s) p -> try (kw <$ symbol s) <|> p) parserZero (M.toList kwMap)

parseSymbol :: Parser Symbol
parseSymbol =
  foldr (\(sym, s) p -> try (sym <$ symbol s) <|> p) parserZero (M.toList symMap)

term :: Parser Terminal
term =
  (IntCons <$> try natural)
  <|> (StringCons <$> try stringLiteral)
  <|> (Identifier <$> try identifier)
  <|> (Keyword <$> parseKeyword)
  <|> (Symbol <$> parseSymbol)

tokenizeLine :: Parser [Terminal]
tokenizeLine = do
  optional comment
  terms <- many term
  termss <- many {- for this abuse -} (comment >> {- of block cmts -} many term)
  return $ concat (terms : termss)

tokenizeSrc :: Parser [Terminal]
tokenizeSrc = do
  ls <- tokenizeLine `sepBy` oneOf "\n\r"
  return $ concat ls
