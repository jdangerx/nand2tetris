module Main where

import Data.Char (isSpace)
import Text.Parsec
import Text.Parsec.Char
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

data Segment = Argument
             | Local
             | Static
             | Constant
             | This
             | That
             | Pointer
             | Temp

type Index = Maybe Int


class Hackable a where
  toHack :: a -> String

comment :: Parser String
comment = do
  string "//"
  many $ noneOf "\n"

spaces1 :: Parser ()
spaces1 = do
  satisfy isSpace
  spaces

segment :: Parser Segment
segment =
  (try (string "argument") >> return Argument)
  <|> (try (string "local") >> return Local)
  <|> (try (string "Static") >> return Static)
  <|> (try (string "Constant") >> return Constant)
  <|> (try (string "This") >> return This)
  <|> (try (string "That") >> return That)
  <|> (try (string "Pointer") >> return Pointer)
  <|> (try (string "Temp") >> return Temp)

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

main :: IO ()
main = return ()
