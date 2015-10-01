#! /usr/bin/env runhaskell
module Main where

import Data.Char (ord)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust, isJust)
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
  getST (Class _ cvds _) = foldl (flip updateST) M.empty cvds

initSRScope :: M.Map String SymbolInfo
initSRScope = M.empty

initMethodScope :: M.Map String SymbolInfo
initMethodScope = M.fromList [("this", SymInf IntT ArgumentK 0)]

instance Scope SubroutineDec where
  getST (SubroutineDec ft _ _ params (SubroutineBody vardecs _))
    = let initScope = if ft == MethodF then initMethodScope else initSRScope
      in foldl (flip updateST) (foldl (flip updateST) initScope params) vardecs

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
      compSt@(CompSt {classNameOf = className}) <- get
      let fullyQualified = className ++ "." ++ ident
      let funcInit =
            ("function " ++ fullyQualified ++ " " ++ show (numLocals srDec))
            : makeConstructorInit compSt srDec
            ++ makeMethodInit srDec
      (funcInit ++) <$> toVM body

makeMethodInit :: SubroutineDec -> [String]
makeMethodInit (SubroutineDec MethodF _ _ _ _) =
  ["push argument 0", "pop pointer 0"]
makeMethodInit _ = []

makeConstructorInit :: CompSt -> SubroutineDec -> [String]
makeConstructorInit
  (CompSt {classScopeOf = classScope})
  (SubroutineDec ConstructorF _ _ _ _) =
    let numFields = M.size . M.filter ((== FieldK) . kindOf) $ classScope
    in [ "push constant " ++ show numFields
       , "call Memory.alloc 1"
       , "pop pointer 0" ]
makeConstructorInit _ _ = []


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
    (++ popVar compSt ident) <$> toVM expr
  toVM (LetStmt arr (Just index) expr) = do
    compSt <- get
    let pushedvar = pushVar compSt arr
    (pushedvar ++ ) <$> (concat <$> sequence [toVM index
                                             , pure ["add"]
                                             , pure ["pop pointer 1"]
                                             , toVM expr
                                             , pure ["pop that 0"]])
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

instance VMWriter SubroutineCall where
  toVM (NakedSubrCall ident exprs) = do
    CompSt {classNameOf = className } <- get
    let this = ["push pointer 0"]
    let numParams = length exprs + 1
    concat <$> mapM toVM exprs
      >>= (pure . (this ++))
      >>= (pure . (++ ["call " ++ className ++ "." ++ ident ++ " " ++ show numParams]))
  toVM (CompoundSubrCall pident ident exprs) = do
    compSt <- get
    let parent = lookupCompSt compSt pident
    let isMethod = isJust parent
    let this = if isMethod
               then pushVar compSt pident
               else []
    let numParams = length exprs + if isMethod then 1 else 0
    let callName = case parent of
          Just (SymInf (ClassName className) _ _) -> className ++ "." ++ ident
          Nothing -> pident ++ "." ++ ident
    concat <$> mapM toVM exprs
      >>= (pure . (this ++))
      >>= (pure . (++ ["call " ++ callName ++ " " ++ show numParams]))

instance VMWriter Expression where
  toVM (Expression term []) = toVM term
  toVM (Expression term rest) =
    let foo (op', term') = concat <$> sequence [toVM term', toVM op']
    in concat <$> sequence (toVM term : map foo rest)

instance VMWriter Term where
  toVM (IntTerm int) = pure ["push constant " ++ show int]
  toVM (StrTerm str) =
    let appends = concatMap (\c -> [ "push constant " ++ show (ord c)
                                  , "call String.appendChar 2"]) str
    in pure ([ "push constant " ++ show (length str) , "call String.new 1"]
             ++ appends)
  toVM (KwTerm FalseKW) = pure ["push constant 0"]
  toVM (KwTerm Null) = pure ["push constant 0"]
  toVM (KwTerm TrueKW) = pure ["push constant 1", "neg"]
  toVM (KwTerm This) = pure ["push pointer 0"]
  toVM (VarName ident) = get >>= pure . flip pushVar ident
  toVM (ArrInd arr ind) = do
    compSt <- get
    toVM ind
      >>= (pure . (++
                   (pushVar compSt arr ++
                    [ "add" , "call Memory.peek 1" ])))
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

pushVar :: CompSt -> String -> [String]
pushVar compSt str =
  let
    SymInf {kindOf = kind, indexOf = index} = fromJust $ lookupCompSt compSt str
  in
   case kind of
    FieldK -> [ "push pointer 0"
             , "push constant " ++ show index
             , "add"
             , "call Memory.peek 1"] -- pushes value in memory to stack
    _ -> ["push " ++ kindToSeg kind ++ " " ++ show index]


popVar :: CompSt -> String -> [String]
popVar compSt str =
  let
    SymInf {kindOf = kind, indexOf = index} = fromJust $ lookupCompSt compSt str
  in
   case kind of
    FieldK -> [ "pop temp 0" -- poppedValue temporarily stored
             , "push pointer 0"
             , "push constant " ++ show index
             , "add" -- now stack looks like fieldAddress : restOfStack
             , "push temp 0" -- poppedValue : fieldAddress : restOfStack
             , "call Memory.poke 2"] -- stores a value in a place
    _ -> ["pop " ++ kindToSeg kind ++ " " ++ show index]


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
     let fstate =
           fmap runIdentity . execStateT <$> (toVM <$> cls) <*> pure initCompSt
     case fstate of
      Right st -> print (classScopeOf st)
      Left err -> print err
     case vm of
      Right stmts -> writeFile outfile (unlines stmts) -- >> print stmts
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
