module Main where

import VMParse
import Text.Parsec
import System.Environment
import System.FilePath

decSP :: [String]
decSP = ["@SP", "M=M-1", "A=M"]

incSP :: [String]
incSP = ["@SP", "M=M+1"]

pushDSP :: [String]
pushDSP = ["@SP", "A=M", "M=D"] ++ incSP

pushSeg :: Segment -> String
pushSeg (Constant (Just n)) =
  unlines ["@" ++ show n, "D=A"]
pushSeg (Local (Just n)) =
  unlines ["@LCL", "D=M", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (Argument (Just n)) =
  unlines ["@ARG", "D=M", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (This (Just n)) =
  unlines ["@THIS", "D=M", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (That (Just n)) =
  unlines ["@THAT", "D=M", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (Temp (Just n)) =
  unlines ["@5", "D=A", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (Pointer (Just n)) =
  unlines ["@THIS", "D=A", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (Static filename (Just n)) =
  unlines ["@" ++ filename ++ "." ++ show n, "D=M"] 
pushSeg _ = ""

popToR13 :: [String]
popToR13 = ["@SP", "M=M-1", "A=M", "D=M", "@R13", "M=D"] -- R13 = **SP

putR13IntoR14 :: [String]
putR13IntoR14 = ["@R13", "D=M", "@R14", "A=M", "M=D"]

popSeg :: Segment -> String
popSeg (Local (Just n)) =
  unlines ["@LCL", "D=M", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (Argument (Just n)) =
  unlines ["@ARG", "D=M", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (This (Just n)) =
  unlines ["@THIS", "D=M", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (That (Just n)) =
  unlines ["@THAT", "D=M", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (Temp (Just n)) =
  unlines ["@5", "D=A", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (Pointer (Just n)) =
  unlines ["@THIS", "D=A", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (Static filename (Just n)) =
  unlines ["@" ++ filename ++ "." ++ show n, "D=A", "@R14", "M=D"]
popSeg _ = ""

class Assemblable a where
  toAsm :: a -> String

instance Assemblable Command where
  toAsm (Push seg) =
    pushSeg seg ++ unlines pushDSP
  toAsm (Pop seg) =
    unlines popToR13 ++ popSeg seg ++ unlines putR13IntoR14
  toAsm Add =
    unlines $ decSP ++ "D=M" : decSP ++ "M=D+M" : incSP
  toAsm Sub =
    unlines $ decSP ++ "D=M" : decSP ++ "M=M-D" : incSP
  toAsm Neg = unlines $ decSP ++ "M=-M" : incSP
  toAsm (Eq jId) =
    unlines $
    decSP ++
    "D=M" : -- D = M = y
    decSP ++
    [
      "D=M-D", -- D = x - y
      "@_eqJmp"++show jId,
      "D;JEQ", -- x = x - y
      "@SP",
      "A=M",
      "M=0",
      "@_endEqJmp"++show jId,
      "0;JMP",
      "(_eqJmp"++show jId++")",
      "@SP",
      "A=M",
      "M=-1",
      "(_endEqJmp"++show jId++")"
    ] ++
    incSP
  toAsm (Gt jId) =
    unlines $
    decSP ++
    "D=M" : -- D = M = y
    decSP ++
    [
      "D=M-D", -- D = x - y
      "@_gtJmp"++show jId,
      "D;JGT", -- x = x - y
      "@SP",
      "A=M",
      "M=0",
      "@_endGtJmp"++show jId,
      "0;JMP",
      "(_gtJmp"++show jId++")",
      "@SP",
      "A=M",
      "M=-1",
      "(_endGtJmp"++show jId++")"
    ] ++
    incSP
  toAsm (Lt jId) =
    unlines $
    decSP ++
    "D=M" : -- D = M = y
    decSP ++
    [
      "D=M-D", -- D = x - y
      "@_ltJmp"++show jId,
      "D;JLT", -- x = x - y
      "@SP",
      "A=M",
      "M=0",
      "@_endLtJmp"++show jId,
      "0;JMP",
      "(_ltJmp"++show jId++")",
      "@SP",
      "A=M",
      "M=-1",
      "(_endLtJmp"++show jId++")"
    ] ++
    incSP
  toAsm And =
    unlines $ decSP ++ "D=M" : decSP ++ "M=D&M" : incSP
  toAsm Or =
    unlines $ decSP ++ "D=M" : decSP ++ "M=D|M" : incSP
  toAsm Not = unlines $ decSP ++ "M=!M" : incSP

main :: IO ()
main = do
  [infile, outfile] <- getArgs
  input <- readFile infile
  let vmCmds = runParser parseVM (takeBaseName infile, 0) infile input
  case vmCmds of
   Left err -> print err
   Right c -> writeFile outfile (unlines . map toAsm $ c)
  return ()
