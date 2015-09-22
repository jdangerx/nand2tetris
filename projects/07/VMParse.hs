module VMParse where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String

data Command = Push Segment
             | Pop Segment
             | Add
             | Sub
             | Neg
             | Eq Filename JumpId
             | Gt Filename JumpId
             | Lt Filename JumpId
             | And
             | Or
             | Not
             deriving Show

data Segment = Argument Index
             | Local Index
             | Static Filename Index
             | Constant Index
             | This Index
             | That Index
             | Pointer Index
             | Temp Index
             deriving Show

type Index = Maybe Int

type Filename = String

type JumpId = Int

comment :: Parsec String (Filename, Int) String
comment = do
  string "//"
  many $ noneOf "\n\r"

spaces1 :: Parsec String (Filename, Int) ()
spaces1 = do
  satisfy isSpace
  spaces

segment :: Parsec String (Filename, Int) (Index -> Segment)
segment =
  (try (string "argument") >> return Argument)
  <|> (try (string "local") >> return Local)
  <|> try (pure Static <*> (string "static" >> fst <$> getState))
  <|> (try (string "constant") >> return Constant)
  <|> (try (string "this") >> return This)
  <|> (try (string "that") >> return That)
  <|> (try (string "pointer") >> return Pointer)
  <|> (try (string "temp") >> return Temp)

index :: Parsec String (Filename, Int) Index
index = do
  num <- optionMaybe (many digit)
  return $ read <$> num

pushPop :: Parsec String (Filename, Int) Command
pushPop = do
  cmd <- (try (string "push") >> return Push) <|> (try (string "pop") >> return Pop)
  spaces1
  seg <- segment
  spaces1
  ind <- index
  return $ cmd . seg $ ind

arithmetic :: Parsec String (Filename, Int) Command
arithmetic =
  try (string "add" >> return Add)
  <|> try (string "sub" >> return Sub)
  <|> try (string "neg" >> return Neg)
  <|> try (pure (uncurry Eq) <*>
           (string "eq" >> modifyState ((+ 1) <$>) >> getState))
  <|> try (pure (uncurry Gt) <*>
           (string "gt" >> modifyState ((+ 1) <$>) >> getState))
  <|> try (pure (uncurry Lt) <*>
           (string "lt" >> modifyState ((+ 1) <$>) >> getState))
  <|> try (string "and" >> return And)
  <|> try (string "or" >> return Or)
  <|> try (string "not" >> return Not)

command :: Parsec String (Filename, Int) (Maybe Command)
command =
  do
    cmd <- optionMaybe (try arithmetic <|> try pushPop)
    optional comment
    return cmd

parseVM :: Parsec String (Filename, Int) [Command]
parseVM =
  do
    cmds <- sepBy command (oneOf "\n\r") <* eof
    return $ catMaybes cmds
