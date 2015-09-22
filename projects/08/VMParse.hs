module VMParse where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Text.Parsec

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
             | Label String Filename
             | Goto String Filename
             | IfGoto String Filename
             | Call String Int Filename JumpId
             | Return
             | Function String Int Filename
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


flow :: Parsec String (Filename, Int) Command
flow =
  do
    cmd <- (string "label" >> return Label)
          <|> (string "goto" >> return Goto)
          <|> (string "if-goto" >> return IfGoto)
    spaces1
    labelName <- many (alphaNum <|> oneOf "_.$:")
    filename <- fst <$> getState
    return $ cmd labelName filename

parseReturn :: Parsec String (Filename, Int) Command
parseReturn = string "return" >> return Return

parseFunction :: Parsec String (Filename, Int) Command
parseFunction =
  do
    string "function"
    spaces1
    funcName <- many (alphaNum <|> oneOf "_.$:")
    spaces1
    numArgs <- read <$> many digit
    (filename, _) <- getState
    return $ Function funcName numArgs filename

parseCall :: Parsec String (Filename, Int) Command
parseCall =
  do
    string "call"
    spaces1
    funcName <- many (alphaNum <|> oneOf "_.$:")
    spaces1
    numArgs <- read <$> many digit
    modifyState ((+ 1) <$>)
    (filename, jId) <- getState
    return $ Call funcName numArgs filename jId

command :: Parsec String (Filename, Int) (Maybe Command)
command =
  do
    cmd <- optionMaybe (try arithmetic
                       <|> try pushPop
                       <|> try flow
                       <|> try parseReturn
                       <|> try parseFunction
                       <|> try parseCall
                       )
    optional comment
    many (noneOf "\n\r")
    return cmd

parseVM :: Parsec String (Filename, Int) [Command]
parseVM =
  do
    cmds <- sepBy command (oneOf "\n\r") <* eof
    return $ catMaybes cmds
