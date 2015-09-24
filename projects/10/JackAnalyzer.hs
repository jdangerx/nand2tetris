module Main where

import Data.Functor.Identity (Identity)
import qualified Data.Map as M
import System.Environment
import System.FilePath
import Text.Parsec.Pos
import Text.Parsec.Prim
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

data ClassVarDec =
  ClassVarDec ClassVarType Type [Identifier]

data ClassVarType = StaticVar | FieldVar
data Type = IntT | CharT | BoolT | ClassName Identifier

data SubroutineDec =
  SubroutineDec FuncType RetType Identifier [Parameter] SubroutineBody

data Parameter = Param Type Identifier

data FuncType = ConstructorF | FunctionF | MethodF

data RetType = VoidT | RetType Type

data SubroutineBody =
  SubroutineBody [VarDec] [Statement]

data VarDec = VarDec Type [Identifier]

data Statement =
  LetStmt Identifier (Maybe Expression) Expression
  | IfStmt Expression [Statement] (Maybe [Statement])
  | WhileStmt Expression [Statement]
  | DoStmt SubroutineCall
  | RetStmt (Maybe Expression)

data Expression = Expression Term [(Op, Term)]

data Term = IntTerm IntCons
          | StrTerm StringCons
          | KwTerm Keyword
          | VarName Identifier
          | ArrInd Identifier Expression
          | SubrCall SubroutineCall
          | Expr Expression
          | UnOp UnaryOp Term

data SubroutineCall = NakedSubrCall SubrName [Expression]
                    | CompoundSubrCall Identifier SubrName [Expression]

data UnaryOp = Neg | Not
             deriving (Show, Eq, Ord)

data Op = Add | Sub | Mul | Div | And | Or | Lt | Gt | Equals
        deriving (Show, Eq, Ord)

opMap :: M.Map Op Terminal
opMap = M.fromList
  [ (Add, Symbol Plus), (Sub, Symbol Minus), (Mul, Symbol Star)
  , (Div, Symbol Slash), (And, Symbol Amp), (Or, Symbol Pipe)
  , (Lt, Symbol LAngle), (Gt, Symbol RAngle), (Equals, Symbol Eq) ]

type SubrName = Identifier

parseIdent :: Parser Identifier
parseIdent = do
  Identifier s <- satisfyT isIdentifier
  return s

parseClass :: Parser Class
parseClass =
  Class <$> (termIs (Keyword ClassKW) *> parseIdent)
  <*> (oBrace *> many classVarDec)
  <*> (many subrDec <* cBrace)

classVarDec :: Parser ClassVarDec
classVarDec =
  ClassVarDec <$> classVarType <*> parseType
  <*> parseIdent `sepBy` termIs (Symbol Comma)

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
  DoStmt <$> (termIs (Keyword Do) *> subrCall <* termIs (Symbol Semi))

returnStmt :: Parser Statement
returnStmt = RetStmt <$> (termIs (Keyword Return)
             *> optionMaybe expression
             <* termIs (Symbol Semi))

expression :: Parser Expression
expression = do
  t <- term
  rest <- many (do
                  op' <- op
                  t' <- term
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

term :: Parser Term
term = try singletonTerm
       <|> try arrayIndex
       <|> try (SubrCall <$> subrCall)
       <|> try (Expr <$> expression)
       <|> try (UnOp <$> unaryOp <*> term)

subrCall :: Parser SubroutineCall
subrCall =
  do
    parentName <- optionMaybe (parseIdent <* termIs (Symbol Dot))
    srName <- parseIdent
    termIs $ Symbol OParen
    exprs <- expressionList
    termIs $ Symbol CParen
    case parentName of
     Just pn -> return $ CompoundSubrCall pn srName exprs
     Nothing -> return $ NakedSubrCall srName exprs

unaryOp :: Parser UnaryOp
unaryOp = try (Neg <$ termIs (Symbol Minus))
          <|> try (Not <$ termIs (Symbol Tilde))

op :: Parser Op
op = parseFromMap termIs opMap

main :: IO ()
main = do
  [infile] <- getArgs
  let outfile = (++ "T1.xml") . dropExtension $ infile
  input <- readFile infile
  let toks = runParser tokenizeSrc () infile input
  case toks of
   Left err -> print err
   Right t -> writeFile outfile $
             newlineToReturn $
             "<tokens>\n" ++ unlines (toXML <$> t) ++ "</tokens>\n"


