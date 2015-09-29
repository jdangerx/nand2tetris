#! /usr/bin/env runhaskell
module Main where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import System.Directory
import System.Environment
import System.FilePath

import JackAnalyzer
import Tokenizer

data Kind = StaticK | FieldK | ArgumentK | VariableK
          deriving (Show, Eq)

kindToSeg :: Kind -> String
kindToSeg StaticK = "static"
kindToSeg FieldK = "field"
kindToSeg ArgumentK = "argument"
kindToSeg VariableK = "local"

type SymbolTable = M.Map String SymbolInfo

data SymbolInfo = SymInf { typeOf :: Type
                         , kindOf :: Kind
                         , indexOf :: Int }
                  deriving Show

class Scope a where
  getST :: a -> SymbolTable

newDef :: String -> Type -> Kind -> SymbolTable -> SymbolTable
newDef ident t k st = M.insert ident (SymInf t k (numOfKind st k)) st

numOfKind :: SymbolTable -> Kind -> Int
numOfKind st k = M.size . M.filter ((== k) . kindOf) $ st

class SymbolDef a where
  updateST :: a -> SymbolTable -> SymbolTable

instance SymbolDef ClassVarDec where
  updateST (ClassVarDec cvt t ids) st =
    let kind StaticVar = StaticK
        kind FieldVar = FieldK
    in foldl (\oldST ident -> newDef ident t (kind cvt) oldST) st ids

instance SymbolDef Parameter where
  updateST (Param t ident) = newDef ident t ArgumentK

instance SymbolDef VarDec where
  updateST (VarDec t idents) st =
    foldl (\oldST ident -> newDef ident t VariableK oldST) st idents

instance Scope Class where
  getST (Class _ cvds _) = foldr updateST M.empty cvds

initSRScope :: M.Map String SymbolInfo
initSRScope = M.fromList [("this", SymInf IntT ArgumentK 0)]

instance Scope SubroutineDec where
  getST (SubroutineDec _ _ _ params (SubroutineBody vardecs _))
    = foldl (flip updateST) (foldl (flip updateST) initSRScope params) vardecs

data Scopz = Scopz { classScope :: SymbolTable
                   , srScope :: SymbolTable
                   , className :: String
                   , srState :: Maybe SRState}

data SRState = SRState { retType :: RetType
                       , fType :: FuncType
                       , jumpId :: Int}

lookupScopz :: Scopz -> String -> Maybe SymbolInfo
lookupScopz (Scopz clsScope srScope' _ _) ident =
  let srSymInfo = M.lookup ident srScope'
      clsSymInfo = M.lookup ident clsScope
  in
   case srSymInfo of
    Nothing -> clsSymInfo
    _ -> srSymInfo

pushVar :: Scopz -> String -> Maybe [String]
pushVar scopz ident =
  (\l -> ["push " ++ kindToSeg (kindOf l) ++ " " ++ show (indexOf l)])
  <$> lookupScopz scopz ident

class VMWriter a where
  toVM :: Scopz -> a -> [String]

instance VMWriter Class where
  toVM scopz (Class ident _ srDecs) =
    let newScopz = scopz { className = ident }
    in
     srDecs >>= toVM newScopz

instance VMWriter SubroutineDec where
  toVM scopz srDec@(SubroutineDec ft rt ident params (SubroutineBody _ stmts)) =
    let newScopz = scopz { srScope = getST srDec
                         , srState = Just SRState { retType = rt
                                                  , fType = ft
                                                  , jumpId = 0 } } 
        funcName = className scopz ++ "." ++ ident
    in ("function " ++ funcName ++ " " ++ show (length params))
       : (stmts >>= toVM newScopz)

instance VMWriter Statement where
  toVM scopz (DoStmt subRCall) = toVM scopz subRCall
  toVM scopz (RetStmt maybeExp) =
    let srState' = srState scopz
        retType' = retType <$> srState'
    in
     case retType' of
      Just VoidT -> ["push constant 0", "return"]
      Just _ -> fromMaybe [] (toVM scopz <$> maybeExp) ++ ["return"]
      Nothing -> error "Return statement encountered outside of function"

instance VMWriter Expression where
  toVM scopz (Expression term' []) = toVM scopz term'
  toVM scopz (Expression term' rest) = toVM scopz term'
                                       ++ (rest >>= opAndTermToVM scopz)

opAndTermToVM :: Scopz -> (Op, Term) -> [String]
opAndTermToVM scopz (op', term') = toVM scopz term' ++ toVM scopz op'

instance VMWriter Term where
  toVM _ (IntTerm int) = ["push constant " ++ show int]
  toVM _ (StrTerm _) = ["STRING TERM NOT SUPPORTED YET"]
  toVM _ (KwTerm FalseKW) = ["push constant 0"]
  toVM _ (KwTerm Null) = ["push constant 0"]
  toVM _ (KwTerm TrueKW) = ["push constant 1", "neg"]
  toVM _ (KwTerm kw) = ["KEYWORD NOT SUPPORTED: " ++ show kw]
  toVM scopz (VarName ident) = fromMaybe [] (pushVar scopz ident)
  toVM _ (ArrInd ident expr') =
    ["ARRAY INDEXING NOT SUPPORTED: " ++ show ident ++ "[" ++ show expr' ++ "]"]
  toVM scopz (SubrCall subRCall) = toVM scopz subRCall
  toVM scopz (Expr expr') = toVM scopz expr'
  toVM scopz (UnOp op' term') = toVM scopz term' ++ toVM scopz op'

instance VMWriter SubroutineCall where
  toVM scopz (NakedSubrCall ident exprs) =
    (exprs >>= toVM scopz) ++ ["call " ++ ident ++ " " ++ show (length exprs)]
  toVM scopz (CompoundSubrCall pident ident exprs) =
    let
      subrName = pident ++ "." ++ ident
    in
     (exprs >>= toVM scopz) ++ ["call " ++ subrName ++ " " ++ show (length exprs)]

instance VMWriter UnaryOp where
  toVM _ Neg = ["neg"]
  toVM _ Not = ["not"]

instance VMWriter Op where
  toVM _ Add = ["add"]
  toVM _ Sub = ["sub"]
  toVM _ Mul = ["call Math.multiply 2"]
  toVM _ Div = ["call Math.divide 2"]
  toVM _ And = ["and"]
  toVM _ Or = ["or"]
  toVM _ Lt = ["lt"]
  toVM _ Gt = ["gt"]
  toVM _ Equals = ["eq"]

writeVM :: FilePath -> IO ()
writeVM infile =
  let outfile = infile -<.> "vm"
  in
   do
     print outfile
     cls <- parseFile infile
     let vm = toVM (Scopz M.empty M.empty "" Nothing) <$> cls
     case vm of
      Right stmts -> writeFile outfile (unlines stmts)
      Left err -> print err

main :: IO ()
main = do
  [infile] <- getArgs
  isFile <- doesFileExist infile
  isDir <- doesDirectoryExist infile
  infiles <- if isFile then return [infile]
            else if isDir then
                   map (combine infile) . filter ((== ".jack") . takeExtension) <$>
                   getDirectoryContents infile
                 else return []
  mapM_ writeVM infiles
