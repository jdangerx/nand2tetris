module Main where

import System.Environment
import System.FilePath
import Text.Parsec

import Tokenizer


newlineToReturn :: String -> String
newlineToReturn = foldr (\c s -> if c == '\n' then '\r':'\n':s else c:s) ""

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

