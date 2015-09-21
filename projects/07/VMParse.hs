module VMParse where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String

data Command = Push Segment Index
             | Pop Segment Index
             | Add
             | Sub
             | Neg
             | Eq
             | Gt
             | Lt
             | And
             | Or
             | Not
             deriving Show

data Segment = Argument
             | Local
             | Static
             | Constant
             | This
             | That
             | Pointer
             | Temp
             deriving Show

type Index = Maybe Int

comment :: Parser String
comment = do
  string "//"
  many $ noneOf "\n\r"

spaces1 :: Parser ()
spaces1 = do
  satisfy isSpace
  spaces

segment :: Parser Segment
segment =
  (try (string "argument") >> return Argument)
  <|> (try (string "local") >> return Local)
  <|> (try (string "static") >> return Static)
  <|> (try (string "constant") >> return Constant)
  <|> (try (string "this") >> return This)
  <|> (try (string "that") >> return That)
  <|> (try (string "pointer") >> return Pointer)
  <|> (try (string "temp") >> return Temp)

index :: Parser Index
index = do
  num <- optionMaybe (many digit)
  return $ read <$> num

pushPop :: Parser Command
pushPop = do
  cmd <- (string "push" >> return Push) <|> (string "pop" >> return Pop)
  spaces1
  seg <- segment
  spaces1
  ind <- index
  return $ cmd seg ind

arithmetic :: Parser Command
arithmetic =
  try (string "add" >> return Add)
  <|> try (string "sub" >> return Sub)
  <|> try (string "neg" >> return Neg)
  <|> try (string "eq" >> return Eq)
  <|> try (string "gt" >> return Gt)
  <|> try (string "lt" >> return Lt)
  <|> try (string "and" >> return And)
  <|> try (string "or" >> return Or)
  <|> try (string "not" >> return Not)

command :: Parser (Maybe Command)
command =
  do
    cmd <- optionMaybe (try arithmetic <|> try pushPop)
    optional comment
    return cmd

parseVM :: Parser [Command]
parseVM =
  do
    cmds <- sepBy command (oneOf "\n\r") <* eof
    return $ catMaybes cmds
