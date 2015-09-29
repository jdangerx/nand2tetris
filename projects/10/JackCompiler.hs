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

lookupScopz :: String -> Scopz -> Maybe SymbolInfo
lookupScopz ident (Scopz clsScope srScope' _ _) =
  let srSymInfo = M.lookup ident srScope'
      clsSymInfo = M.lookup ident clsScope
  in
   case srSymInfo of
    Nothing -> clsSymInfo
    _ -> srSymInfo

class VMWriter a where
  toVM :: Scopz -> a -> [String]

instance VMWriter Class where
  toVM _ cls@(Class ident cvDecs srDecs) =
    let newScopz = Scopz (getST cls) M.empty ident Nothing
    in srDecs >>= toVM newScopz

instance VMWriter SubroutineDec where
  toVM scopz srDec@(SubroutineDec ft rt ident params body) =
    let newScopz = scopz { srScope = getST srDec
                         , srState = Just (SRState rt ft 0) }
    in [ "function " ++ className scopz ++ "." ++ ident ++ " " ++ show (numLocals srDec)
       ] ++
       toVM newScopz body

numLocals :: SubroutineDec -> Int
numLocals (SubroutineDec _ _ _ params (SubroutineBody varDecs _)) =
  let numParams = length params
      numVarsInDec (VarDec _ ids) = length ids
      numVars = sum . map numVarsInDec $ varDecs
  in
   numParams + numVars + 1


instance VMWriter SubroutineBody where
  toVM scopz (SubroutineBody _ statements) =
    snd (foldl
         (\(scopz', vm) stmt ->
           (incJId scopz', vm ++ toVM scopz' stmt))
         (scopz, [])
         statements)

incJId :: Scopz -> Scopz
incJId scopz@(Scopz _ _ _ (Just srState'@(SRState _ _ jid))) =
  scopz {srState = Just srState' {jumpId = jid + 1}}
incJId scopz@(Scopz _ _ _ Nothing) = scopz

getJId :: Scopz -> Maybe Int
getJId (Scopz _ _ _ (Just (SRState _ _ jid))) = Just jid
getJId (Scopz _ _ _ Nothing) = Nothing

instance VMWriter Statement where
  toVM scopz (LetStmt ident exprMaybe expr) =
    case exprMaybe of
     Nothing -> toVM scopz expr ++ [popVar ident scopz]
     Just indExpr -> toVM scopz expr ++
                    toVM scopz indExpr ++
                    ["ARRAY STUFF HERE"]
  toVM scopz (IfStmt pred' ifBlock elseBlockMaybe) =
    let jid = fromJust $ getJId scopz
        endIf = className scopz ++ "__" ++ "endif__" ++ show jid
    in
     toVM scopz pred' ++ ["not", "if-goto " ++ endIf] ++
     (ifBlock >>= toVM scopz) ++ ["label " ++ endIf] ++
     fromMaybe [] (concatMap (toVM scopz) <$> elseBlockMaybe)
  toVM scopz (WhileStmt pred' block) =
    let jid = fromJust $ getJId scopz
        startWhile = className scopz ++ "__" ++ "startWhile__" ++ show jid
        endWhile = className scopz ++ "__" ++ "endWhile__" ++ show jid
    in
     ["label " ++ startWhile] ++ toVM scopz pred' ++
     ["not", "if-goto " ++ endWhile] ++ (block >>= toVM scopz) ++
     ["goto " ++ startWhile, "label " ++ endWhile]
  toVM scopz (DoStmt srcall) = toVM scopz srcall
  toVM scopz (RetStmt exprMaybe) =
    fromMaybe ["push constant 0"] (toVM scopz <$> exprMaybe) ++ ["return"]

pushVar :: String -> Scopz -> String
pushVar ident scopz = fromMaybe "" (("push " ++) <$> varLoc ident scopz)

popVar :: String -> Scopz -> String
popVar ident scopz = fromMaybe "" (("pop " ++) <$> varLoc ident scopz)

varLoc :: String -> Scopz -> Maybe String
varLoc ident scopz =
  case ident `lookupScopz` scopz of
   Nothing -> Nothing
   Just (SymInf _ k i) -> Just (kindToSeg k ++ " " ++ show i)

instance VMWriter Expression where
  toVM scopz (Expression term rest) =
    toVM scopz term ++
    (rest >>= (\(op', term') -> toVM scopz term' ++ toVM scopz op'))

instance VMWriter Term where
  toVM _ (IntTerm i) = ["push constant " ++ show i]
  -- string term
  toVM scopz (VarName ident) =
    fromMaybe [] ((:[]) . ("push " ++) <$> varLoc ident scopz)
  toVM scopz (ArrInd ident expr') =
    [pushVar ident scopz]
    ++ toVM scopz expr'
    ++ ["add", "pop pointer 1", "pop that 0"]
  toVM scopz (SubrCall srcall) = toVM scopz srcall
  toVM scopz (Expr expr') = toVM scopz expr'
  toVM scopz (UnOp op' term') = toVM scopz term' ++ toVM scopz op'
  toVM _ (KwTerm This) = ["push pointer 0"]
  toVM _ (KwTerm FalseKW) = ["push constant 0"]
  toVM _ (KwTerm TrueKW) = ["push constant 1", "neg"]
  toVM _ (KwTerm Null) = ["push constant 0"]
  toVM _ term' = ["unsupported term type: " ++ show term']

instance VMWriter SubroutineCall where
  toVM scopz (NakedSubrCall ident exprs) =
    ["push pointer 0"] ++ 
    (exprs >>= toVM scopz) ++
    ["call " ++ ident ++ " " ++ show (length exprs)]
  toVM scopz (CompoundSubrCall pident ident exprs) =
    ["push pointer 0"] ++
    (exprs >>= toVM scopz) ++
    ["call " ++ pident ++ "." ++ ident ++ " " ++ show (length exprs)]

instance VMWriter UnaryOp where
  toVM _ Neg = ["neg"]
  toVM _ Not = ["not"]

instance VMWriter Op where
  toVM _ Add = ["add"]
  toVM _ Sub = ["sub"]
  toVM _ Mul = ["call Math.multiply 2"]
  toVM _ Div = ["call Math.divide 2"]
  toVM _ Equals = ["eq"]
  toVM _ Gt = ["gt"]
  toVM _ Lt = ["lt"]
  toVM _ And = ["and"]
  toVM _ Or = ["or"]

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
  -- let toBeProcessed = (map fst . filter ((== ".jack") . snd) . map splitExtension $ infiles)
  print infile
  print infiles
  -- print toBeProcessed
  mapM_ writeVM infiles
