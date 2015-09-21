module Main where

import VMParse
import Text.Parsec
import System.Environment

class Assemblable a where
  toAsm :: a -> String

popSP :: [String]
popSP = ["@SP", "M=M-1", "A=M"]
 
pushSP :: [String]
pushSP = ["@SP", "M=M+1"]

instance Assemblable Command where
  toAsm (Push Constant (Just n)) =
    unlines $
    [
      "@" ++ show n, -- A = n
      "D=A",  -- D = n
      "@SP",
      "A=M",
      "M=D"
    ] ++ pushSP
  toAsm Add =
    unlines $
    popSP ++
    "D=M" :
    popSP ++
    "M=D+M" : -- x = y + x
    pushSP
  toAsm Sub =
    unlines $
    popSP ++
    "D=M" : -- D = M = y
    popSP ++
    "M=M-D" :
    pushSP -- x = x - y
  toAsm Neg = unlines $ popSP ++ "M=-M" : pushSP
  toAsm (Eq jId) =
    unlines $
    popSP ++
    "D=M" : -- D = M = y
    popSP ++
    [
      "D=M-D", -- D = x - y
      "@eqJmp"++show jId,
      "D;JEQ", -- x = x - y
      "@SP",
      "A=M",
      "M=0",
      "@endEqJmp"++show jId,
      "0;JMP",
      "(eqJmp"++show jId++")",
      "@SP",
      "A=M",
      "M=-1",
      "(endEqJmp"++show jId++")"
    ] ++
    pushSP
  toAsm (Gt jId) = 
    unlines $
    popSP ++
    "D=M" : -- D = M = y
    popSP ++
    [
      "D=M-D", -- D = x - y
      "@gtJmp"++show jId,
      "D;JGT", -- x = x - y
      "@SP",
      "A=M",
      "M=0",
      "@endGtJmp"++show jId,
      "0;JMP",
      "(gtJmp"++show jId++")",
      "@SP",
      "A=M",
      "M=-1",
      "(endGtJmp"++show jId++")"
    ] ++
    pushSP
  toAsm (Lt jId) = 
    unlines $
    popSP ++
    "D=M" : -- D = M = y
    popSP ++
    [
      "D=M-D", -- D = x - y
      "@ltJmp"++show jId,
      "D;JLT", -- x = x - y
      "@SP",
      "A=M",
      "M=0",
      "@endLtJmp"++show jId,
      "0;JMP",
      "(ltJmp"++show jId++")",
      "@SP",
      "A=M",
      "M=-1",
      "(endLtJmp"++show jId++")"
    ] ++
    pushSP
  toAsm And =
    unlines $
    popSP ++
    "D=M" :
    popSP ++
    "M=D&M" :
    pushSP
  toAsm Or =
    unlines $
    popSP ++
    "D=M" :
    popSP ++
    "M=D|M" :
    pushSP
  toAsm Not = unlines $ popSP ++ "M=!M" : pushSP

main :: IO ()
main = do
  [infile, outfile] <- getArgs
  input <- readFile infile
  let vmCmds = runParser parseVM 0 infile input
  case vmCmds of
   Left err -> print err
   Right c -> writeFile outfile (unlines . map toAsm $ c)
  return ()
