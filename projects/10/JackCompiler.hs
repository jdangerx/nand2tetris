module Main where

import qualified Data.Map as M
import JackAnalyzer

data Kind = StaticK | FieldK | ArgumentK | VariableK
          deriving Eq

type SymbolTable = M.Map String SymbolInfo

data SymbolInfo = SymInf { typeOf :: Type
                         , kindOf :: Kind
                         , indexOf :: Int }

newDef :: SymbolTable -> String -> Type -> Kind -> SymbolTable
newDef st ident t k = M.insert ident (SymInf t k (numOfKind st k)) st

numOfKind :: SymbolTable -> Kind -> Int
numOfKind st k = M.size . M.filter ((== k) . kindOf) $ st

updateSTWithCVDec :: SymbolTable -> ClassVarDec -> SymbolTable
updateSTWithCVDec st (ClassVarDec cvt t ids) =
  let kind StaticVar = StaticK
      kind FieldVar = FieldK
  in foldr (\ident oldST -> newDef oldST ident t (kind cvt)) st ids

main :: IO ()
main = return ()


