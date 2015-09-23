module Main where

import System.Environment
import System.FilePath
import Text.Parsec

import Tokenizer


newlineToReturn :: String -> String
newlineToReturn = foldr (\c s -> if c == '\n' then '\r':'\n':s else c:s) ""

data Class =
  Class Identifier [ClassVarDec] [SubroutineDec]

data ClassVarDec =
  ClassVarDec ClassVarType Type [Identifier]

data ClassVarType = StaticVar | FieldVar
data Type = IntT | CharT | BoolT | ClassName Identifier

data SubroutineDec =
  SubroutineDec FuncType RetType Identifier [Identifier] SubroutineBody

data FuncType = ConstructorF | FunctionF | MethodF

data RetType = Void | RetType Type

data SubroutineBody =
  SubroutineBody [VarDec] [Statement]

data VarDec = VarDec Type [Identifier]

data Statement =
  LetStmt Identifier (Maybe Expression) Expression
  | IfStmt Expression [Statement] (Maybe [Statement])
  | WhileStmt Expression [Statement]
  | DoStmt SubroutineCall
  | RetStm (Maybe Expression)

data Expression = Expression Term [(Op, Term)]

data Term = IntTerm IntCons
          | StrTerm StringCons
          | KwTerm Keyword
          | VarName Identifier
          | ArrInd Identifier Expression
          | SubrCall SubroutineCall
          | Expr Expression
          | UnOp UnaryOp Term

data SubroutineCall = Naked SubrName [Expression]
                    | Method Identifier SubrName [Expression]

data UnaryOp = Neg | Not

data Op = Add | Sub | Mul | Div | And | Or | Lt | Gt | Equals

type SubrName = Identifier


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

