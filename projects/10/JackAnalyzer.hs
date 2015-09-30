#! /usr/bin/env runhaskell
module JackAnalyzer where

import qualified Data.Map as M
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec (ParseError)
import Text.Parsec.Combinator

import Tokenizer

oParen :: Parser Terminal
oParen = termIs $ Symbol OParen

cParen :: Parser Terminal
cParen = termIs $ Symbol CParen

oBrace :: Parser Terminal
oBrace = termIs $ Symbol OBrace

cBrace :: Parser Terminal
cBrace = termIs $ Symbol CBrace

oBracket :: Parser Terminal
oBracket = termIs $ Symbol OBracket

cBracket :: Parser Terminal
cBracket = termIs $ Symbol CBracket

newlineToReturn :: String -> String
newlineToReturn = foldr (\c s -> if c == '\n' then '\r':'\n':s else c:s) ""

type Parser a = Parsec [Terminal] () a

data Class =
  Class Identifier [ClassVarDec] [SubroutineDec]
  deriving Show

data ClassVarDec =
  ClassVarDec ClassVarType Type [Identifier]
  deriving Show

data ClassVarType = StaticVar | FieldVar
                  deriving Show

data Type = IntT | CharT | BoolT | ClassName Identifier
          deriving (Show, Eq)

data SubroutineDec =
  SubroutineDec FuncType RetType Identifier [Parameter] SubroutineBody
  deriving Show

data Parameter = Param Type Identifier
               deriving Show

data FuncType = ConstructorF | FunctionF | MethodF
              deriving (Show, Eq)

data RetType = VoidT | RetType Type
             deriving (Show, Eq)

data SubroutineBody =
  SubroutineBody [VarDec] [Statement]
  deriving Show

data VarDec = VarDec Type [Identifier]
            deriving Show

data Statement =
  LetStmt Identifier (Maybe Expression) Expression
  | IfStmt Expression [Statement] (Maybe [Statement])
  | WhileStmt Expression [Statement]
  | DoStmt SubroutineCall
  | RetStmt (Maybe Expression)
  deriving Show

data Expression = Expression Term [(Op, Term)]
                deriving Show

data Term = IntTerm IntCons
          | StrTerm StringCons
          | KwTerm Keyword
          | VarName Identifier
          | ArrInd Identifier Expression
          | SubrCall SubroutineCall
          | Expr Expression
          | UnOp UnaryOp Term
          deriving Show

data SubroutineCall = NakedSubrCall Identifier [Expression]
                    | CompoundSubrCall Identifier Identifier [Expression]
                    deriving Show

data UnaryOp = Neg | Not
             deriving (Show, Eq, Ord)

data Op = Add | Sub | Mul | Div | And | Or | Lt | Gt | Equals
        deriving (Show, Eq, Ord)

opMap :: M.Map Op Terminal
opMap = M.fromList
  [ (Add, Symbol Plus), (Sub, Symbol Minus), (Mul, Symbol Star)
  , (Div, Symbol Slash), (And, Symbol Amp), (Or, Symbol Pipe)
  , (Lt, Symbol LAngle), (Gt, Symbol RAngle), (Equals, Symbol Eq) ]

parseIdent :: Parser Identifier
parseIdent = do
  Identifier s <- satisfyT isIdentifier
  return s

parseClass :: Parser Class
parseClass =
  Class <$> (termIs (Keyword ClassKW) *> parseIdent)
  <*> (oBrace *> many (classVarDec <?> "class variable declaration"))
  <*> (many (subrDec <?> "subroutine declaration") <* cBrace)

classVarDec :: Parser ClassVarDec
classVarDec =
  ClassVarDec <$> classVarType <*> parseType
  <*> parseIdent `sepBy` termIs (Symbol Comma)
  <* termIs (Symbol Semi)


classVarType :: Parser ClassVarType
classVarType = try (StaticVar <$ termIs (Keyword Static))
               <|> try (FieldVar <$ termIs (Keyword Field))

subrDec :: Parser SubroutineDec
subrDec =
  SubroutineDec <$> parseFuncType <*> parseRetType <*> parseIdent
  <*> between oParen cParen paramList <*> subrBody

parseFuncType :: Parser FuncType
parseFuncType =
  try (ConstructorF <$ termIs (Keyword Constructor))
  <|> try (FunctionF <$ termIs (Keyword Function))
  <|> try (MethodF <$ termIs (Keyword Method))

parseRetType :: Parser RetType
parseRetType =
  try (RetType <$> parseType)
  <|> try (VoidT <$ termIs (Keyword Void))

paramList :: Parser [Parameter]
paramList =
  (Param <$> parseType <*> parseIdent) `sepBy` termIs (Symbol Comma)

parseType :: Parser Type
parseType =
  try (IntT <$ termIs (Keyword Int))
  <|> try (CharT <$ termIs (Keyword Char))
  <|> try (BoolT <$ termIs (Keyword Boolean))
  <|> try (ClassName <$> parseIdent)

subrBody :: Parser SubroutineBody
subrBody = between oBrace cBrace
           (SubroutineBody <$> many varDec <*> many statement)

varDec :: Parser VarDec
varDec =
  VarDec <$> (termIs (Keyword Var) *> parseType)
  <*> (parseIdent `sepBy` termIs (Symbol Comma))
  <* termIs (Symbol Semi)

statement :: Parser Statement
statement = try letStmt
            <|> try ifStmt
            <|> try returnStmt
            <|> try doStmt
            <|> try whileStmt

letStmt :: Parser Statement
letStmt = do
  termIs (Keyword Let)
  varName <- parseIdent
  indexMaybe <- optionMaybe $ between oBracket cBracket expression
  termIs (Symbol Eq)
  expr <- expression
  termIs (Symbol Semi)
  return $ LetStmt varName indexMaybe expr

ifStmt :: Parser Statement
ifStmt = do
  termIs (Keyword If)
  expr <- between oParen cParen expression
  stmts <- between oBrace cBrace (many statement)
  elseStmts <- optionMaybe
              (termIs (Keyword Else) >> between oBrace cBrace (many statement))
  return $ IfStmt expr stmts elseStmts

whileStmt :: Parser Statement
whileStmt = do
  termIs (Keyword While)
  expr <- between oParen cParen expression
  stmts <- between oBrace cBrace (many statement)
  return $ WhileStmt expr stmts

doStmt :: Parser Statement
doStmt =
  DoStmt <$> (termIs (Keyword Do) *> parseSubrCall <* termIs (Symbol Semi))

returnStmt :: Parser Statement
returnStmt = RetStmt <$> (termIs (Keyword Return)
             *> optionMaybe expression
             <* termIs (Symbol Semi))

expression :: Parser Expression
expression = do
  t <- parseTerm
  rest <- many (do
                  op' <- op
                  t' <- parseTerm
                  return (op', t'))
  return $ Expression t rest

expressionList :: Parser [Expression]
expressionList = expression `sepBy` termIs (Symbol Comma)

singletonTerm :: Parser Term
singletonTerm = token show (const $ initialPos "noPos")
                (\t -> case t of
                  IntCons i -> Just (IntTerm i)
                  StringCons str -> Just (StrTerm str)
                  Keyword str -> Just (KwTerm str)
                  Identifier str -> Just (VarName str)
                  _ -> Nothing
                )

arrayIndex :: Parser Term
arrayIndex = ArrInd <$> parseIdent <*> between oBracket cBracket expression

parseTerm :: Parser Term
parseTerm = try (SubrCall <$> parseSubrCall)
            <|> try (Expr <$> between oParen cParen expression)
            <|> try (UnOp <$> unaryOp <*> parseTerm)
            <|> try arrayIndex
            <|> try singletonTerm

parseSubrCall :: Parser SubroutineCall
parseSubrCall =
  try (NakedSubrCall <$> parseIdent <*> between oParen cParen expressionList)
  <|> try (CompoundSubrCall <$> parseIdent
           <*> (termIs (Symbol Dot) *> parseIdent)
           <*> between oParen cParen expressionList)

unaryOp :: Parser UnaryOp
unaryOp = try (Neg <$ termIs (Symbol Minus))
          <|> try (Not <$ termIs (Symbol Tilde))

op :: Parser Op
op = parseFromMap termIs opMap

parseFile :: FilePath -> IO (Either ParseError Class)
parseFile infile = do
  input <- readFile infile
  return $ runParser tokenizeSrc () infile input
    >>= runParser parseClass () infile

-- main :: IO ()
-- main = do
  -- [infile] <- getArgs
  -- isFile <- doesFileExist infile
  -- isDir <- doesDirectoryExist infile
  -- infiles <- if isFile then return [infile]
            -- else if isDir then
                   -- map (combine infile) . filter ((== ".jack") . takeExtension)
                   -- <$> getDirectoryContents infile
                 -- else return []
  -- classes <- mapM parseFile infiles
  -- print classes
  -- return ()
