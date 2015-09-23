module Main where

import System.Environment
import System.FilePath
import Text.Parsec

import Tokenizer


newlineToReturn :: String -> String
newlineToReturn = foldr (\c s -> if c == '\n' then '\r':'\n':s else c:s) ""

-- data Class = Class { className :: ClassName
--                    , classVarDecs :: [ClassVarDec]
--                    , subroutineDecs :: [SubroutineDec] }

-- data ClassVarDec = ClassVarDec { varType :: Type
--                                , varNames :: [VarName] }

-- data Type = IntT
--           | CharT
--           | BoolT
--           | ClassT ClassName

-- data SubroutineDec = SubroutineDec { retType :: Type
--                                    , subroutineName :: SubroutineName
--                                    , paramList :: ParameterList
--                                    , subroutineBody :: SubroutineBody}

-- type ParameterList = [Identifier]

-- data SubroutineBody = SubroutineBody { varDecs :: [VarDec]
--                                      , statements :: [Stmt] }

-- type VarDec = (Type, [VarName])

-- type ClassName = Identifier
-- type SubroutineName = Identifier
-- type VarName = Identifier

-- data Stmt = LetStmt VarName (Maybe Expr) Expr
--           | IfStmt Expr [Stmt] (Maybe [Stmt])
--           | WhileStmt Expr [Stmt]
--           | DoStmt SubroutineCall
--           | ReturnStmt (Maybe Expr)


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

