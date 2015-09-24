module Main where

import Data.Functor.Identity (Identity)
import qualified Data.Map as M
import System.Environment
import System.FilePath
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Combinator

import Tokenizer


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
             deriving (Show, Eq, Ord)

data Op = Add | Sub | Mul | Div | And | Or | Lt | Gt | Equals
        deriving (Show, Eq, Ord)

opMap :: M.Map Op Terminal
opMap = M.fromList
  [ (Add, Symbol Plus), (Sub, Symbol Minus), (Mul, Symbol Star)
  , (Div, Symbol Slash), (And, Symbol Amp), (Or, Symbol Pipe)
  , (Lt, Symbol LAngle), (Gt, Symbol RAngle), (Equals, Symbol Eq) ]

type SubrName = Identifier

-- data Term = IntTerm IntCons
--           | StrTerm StringCons
--           | KwTerm Keyword
--           | VarName Identifier
--           | ArrInd Identifier Expression
--           | SubrCall SubroutineCall
--           | Expr Expression
--           | UnOp UnaryOp Term
subRCall :: Parser SubroutineCall
subRCall =
  do
    parentName <- optionMaybe (satisfyT isIdentifier)
    termIs $ Symbol Dot
    Identifier srName <- satisfyT isIdentifier
    termIs $ Symbol OParen
    -- exprs <- many expression
    termIs $ Symbol CParen
    return $ Naked srName []

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


