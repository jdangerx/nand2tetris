#! /usr/bin/env runhaskell
module Main where

import Data.Functor.Identity (runIdentity)
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Control.Monad.State
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
initSRScope = M.empty
-- initSRScope = M.fromList [("this", SymInf IntT ArgumentK 0)]

instance Scope SubroutineDec where
  getST (SubroutineDec _ _ _ params (SubroutineBody vardecs _))
    = foldl (flip updateST) (foldl (flip updateST) initSRScope params) vardecs

data CompSt = CompSt { classScopeOf :: SymbolTable
                     , srScopeOf :: SymbolTable
                     , classNameOf :: String
                     , srNameOf :: String
                     , retTypeOf :: RetType
                     , funcTypeOf :: FuncType
                     , jumpIdOf :: Int }
initCompSt :: CompSt
initCompSt = CompSt M.empty M.empty "" "" VoidT FunctionF 0

class VMWriter a where
  toVM :: a -> State CompSt [String]

instance VMWriter Class where
  toVM cls@(Class ident _ srDecs) =
    do
      modify (\cs -> cs { classNameOf = ident
                       , classScopeOf = getST cls })
      concat <$> mapM toVM srDecs

instance VMWriter SubroutineDec where
  toVM srDec@(SubroutineDec ft rt ident params body) =
    do
      modify (\cs -> cs { srNameOf = ident
                       , funcTypeOf = ft
                       , retTypeOf = rt
                       , srScopeOf = getST srDec })
      CompSt {classNameOf = className} <- get
      let fullyQualified = className ++ "." ++ ident
      let funcLine =
            ["function " ++ fullyQualified ++ " " ++ show (numLocals srDec)]
      (funcLine ++) <$> toVM body

numLocals :: SubroutineDec -> Int
numLocals (SubroutineDec ft _ _ params (SubroutineBody varDecs _)) =
  let numParams = if ft == MethodF
                  then length params + 1
                  else length params
  in sum ((\(VarDec _ ids) -> length ids) <$> varDecs) + numParams

instance VMWriter SubroutineBody where
  toVM (SubroutineBody _ stmts) = concat <$> mapM toVM stmts

instance VMWriter Statement where
  toVM (LetStmt ident Nothing expr) = do
    compSt <- get
    (++ [popVar compSt ident]) <$> toVM expr
  toVM (IfStmt predicate ifBlock elseBlock) = do
    CompSt {classNameOf = className, srNameOf = srName, jumpIdOf = jumpId} <- get
    modify (\cs -> cs { jumpIdOf = jumpId + 1 })
    let elseLabel = className ++ "." ++ srName ++ "_$_else_$_" ++ show jumpId
    let endifLabel = className ++ "." ++ srName ++ "_$_endif_$_" ++ show jumpId
    concat <$> sequence
      [ toVM predicate
      , pure ["not"]
      , pure ["if-goto " ++ elseLabel]
      , concat <$> mapM toVM ifBlock
      , pure ["goto " ++ endifLabel]
      , pure ["label " ++ elseLabel]
      , concat <$> mapM toVM (fromMaybe [] elseBlock)
      , pure ["label " ++ endifLabel]]
  toVM (WhileStmt predicate block) = do
    CompSt {classNameOf = className, srNameOf = srName, jumpIdOf = jumpId} <- get
    modify (\cs -> cs { jumpIdOf = jumpId + 1 })
    let startLabel = className ++ "." ++ srName ++ "_$_startwhile_$_" ++ show jumpId
    let endLabel = className ++ "." ++ srName ++ "_$_endwhile_$_" ++ show jumpId
    concat <$> sequence
      [ pure ["label " ++ startLabel]
      , toVM predicate
      , pure ["not"]
      , pure ["if-goto " ++ endLabel]
      , concat <$> mapM toVM block
      , pure ["goto " ++ startLabel]
      , pure ["label " ++ endLabel]]
  toVM (DoStmt srCall) = toVM srCall >>= (pure . (++ ["pop temp 0"]))
  toVM (RetStmt maybeExp) =
    do
      CompSt {retTypeOf = retType} <- get
      let ret = if retType == VoidT
                then ["push constant 0", "return"]
                else ["return"]
      case maybeExp of
       Nothing -> pure ret
       Just expr -> toVM expr >>= pure . (++ ret)
  toVM stmt = pure [show stmt ++ "is not supported"]

instance VMWriter SubroutineCall where
  toVM subroutineCall = do
    compSt <- get
    let
      (callName, exprs, ident) =
        case subroutineCall of
         NakedSubrCall ident' exprs' -> (ident', exprs', ident')
         CompoundSubrCall pident ident' exprs' -> (pident ++ "." ++ ident', exprs', ident')
    let isMethod = isNothing $ lookupCompSt compSt ident
    concat <$> mapM toVM exprs
      >>= (pure . (++ ["call " ++ callName ++ " " ++ show (length exprs)]))

instance VMWriter Expression where
  toVM (Expression term []) = toVM term
  toVM (Expression term rest) =
    let foo (op', term') = concat <$> sequence [toVM term', toVM op']
    in concat <$> sequence (toVM term : map foo rest)

instance VMWriter Term where
  toVM (IntTerm int) = pure ["push constant " ++ show int]
  -- strTerm
  toVM (KwTerm FalseKW) = pure ["push constant 0"]
  toVM (KwTerm Null) = pure ["push constant 0"]
  toVM (KwTerm TrueKW) = pure ["push constant 1", "neg"]
  toVM (VarName ident) = get >>= pure . (:[]) . flip pushVar ident
  -- arrInd
  toVM (SubrCall subrCall) = toVM subrCall
  toVM (Expr expr) = toVM expr
  toVM (UnOp op' term) = concat <$> sequence [toVM term, toVM op']
  toVM t = pure [show t ++ " is not supported"]

lookupCompSt :: CompSt -> String -> Maybe SymbolInfo
lookupCompSt (CompSt {classScopeOf = clsScope, srScopeOf = srScope}) ident =
  let srSymInfo = M.lookup ident srScope
      clsSymInfo = M.lookup ident clsScope
  in
   case srSymInfo of
    Nothing -> clsSymInfo
    _ -> srSymInfo

pushVar :: CompSt -> String -> String
pushVar compSt str =
  let
    SymInf {kindOf = kind, indexOf = index} = fromJust $ lookupCompSt compSt str
  in
   "push " ++ kindToSeg kind ++ " " ++ show index

popVar :: CompSt -> String -> String
popVar compSt str =
  let
    SymInf {kindOf = kind, indexOf = index} = fromJust $ lookupCompSt compSt str
  in
   "pop " ++ kindToSeg kind ++ " " ++ show index

instance VMWriter UnaryOp where
  toVM Neg = pure ["neg"]
  toVM Not = pure ["not"]

instance VMWriter Op where
  toVM Add = pure ["add"]
  toVM Sub = pure ["sub"]
  toVM Mul = pure ["call Math.multiply 2"]
  toVM Div = pure ["call Math.divide 2"]
  toVM And = pure ["and"]
  toVM Or = pure ["or"]
  toVM Lt = pure ["lt"]
  toVM Gt = pure ["gt"]
  toVM Equals = pure ["eq"]

writeVM :: FilePath -> IO ()
writeVM infile =
  let outfile = infile -<.> "vm"
  in
   do
     cls <- parseFile infile
     let vm =
           fmap runIdentity . evalStateT <$> (toVM <$> cls) <*> pure initCompSt
     case vm of
      Right stmts -> writeFile outfile (unlines stmts) >> print stmts
      -- Right stmts -> print stmts
      -- Right stmts -> putStrLn
                    -- $ unlines . filter (isInfixOf "not supported") $ stmts
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
