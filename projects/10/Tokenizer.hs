module Tokenizer where

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

type Keyword = String
type Symbol = String
type IntCons = Integer
type StringCons = String
type Identifier = String

toXML :: Terminal -> String
toXML (Keyword s) = "<keyword> " ++ s ++ " </keyword>"
toXML (Symbol ">") = "<symbol> &gt; </symbol>"
toXML (Symbol "<") = "<symbol> &lt; </symbol>"
toXML (Symbol "&") = "<symbol> &amp; </symbol>"
toXML (Symbol s) = "<symbol> " ++ s ++ " </symbol>"
toXML (IntCons i) = "<integerConstant> " ++ show i ++ " </integerConstant>"
toXML (StringCons s) = "<stringConstant> " ++ s ++ "\n</stringConstant>"
toXML (Identifier s) = "<identifier> " ++ s ++ " </identifier>"

jackDef :: P.LanguageDef st
jackDef = javaStyle
          { P.identStart = letter <|> char '_'
          , P.identLetter = alphaNum <|> char '_'
          , P.reservedNames = [ "class" , "constructor" , "function" , "method"
                              , "field" , "static" , "var" , "int" , "char"
                              , "boolean" , "void" , "true" , "false" , "null"
                              , "this" , "let" , "do" , "if" , "else" , "while"
                              , "return" ]
          , P.reservedOpNames = [ "+", "-", "*", "/", "&", "|", "<", ">", "="
                                , "." , "~", ";", ","] }

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

term :: Parser Terminal
term =
  (IntCons <$> try natural)
  <|> (StringCons <$> try stringLiteral)
  <|> (Identifier <$> try identifier)
  <|>(Keyword <$>
   foldr (<|>) parserZero ((try . symbol) <$> P.reservedNames jackDef))
  <|> (Symbol <$>
       foldr (<|>)
       parserZero ((try . symbol) <$>
                   (["{", "}", "[", "]", "(", ")"] ++ P.reservedOpNames jackDef)))


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
