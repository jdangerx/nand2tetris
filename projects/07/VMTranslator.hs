module Main where

import VMParse
import Text.Parsec
import System.Environment

class Assemblable a where
  toAsm :: a -> String

instance Assemblable Command where
  toAsm (Push Constant (Just n)) = unlines
                                   [
                                     "@" ++ show n, -- A = n
                                     "D=A",  -- D = n
                                     "@SP",
                                     "A=M",
                                     "M=D",
                                     "@SP",
                                     "M=M+1"
                                   ]
  toAsm Add = unlines
              [
                "@SP", -- M = &y+1
                "M=M-1", -- M = &y
                "A=M", -- A = &y
                "D=M", -- D = M = y
                "@SP",
                "M=M-1", -- M = *x
                "A=M", -- A = *x
                "M=D+M", -- x = y + x
                "@SP",
                "M=M+1"
              ]
  toAsm Sub = unlines
              [
                "@SP",
                "M=M-1", -- M = &y
                "A=M",
                "D=M", -- D = M = y
                "@SP",
                "M=M-1",
                "A=M", -- M = x
                "M=M-D", -- x = x - y
                "@SP",
                "M=M+1"
              ]
  toAsm Neg = unlines
              [
                "@SP", -- A = &(&y)
                "M=M-1", -- M = &y
                "A=M", -- A = &y
                "M=-M", -- y = -y
                "@SP",
                "M=M+1"
              ]
  toAsm (Eq jId) = unlines
                   [
                     "@SP",
                     "M=M-1", -- M = &y
                     "A=M",
                     "D=M", -- D = M = y
                     "@SP",
                     "M=M-1",
                     "A=M", -- M = x
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
                     "(endEqJmp"++show jId++")",
                     "@SP",
                     "M=M+1"
                   ]
  toAsm (Gt jId) = unlines
                   [
                     "@SP",
                     "M=M-1", -- M = &y
                     "A=M",
                     "D=M", -- D = M = y
                     "@SP",
                     "M=M-1",
                     "A=M", -- M = x
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
                     "(endGtJmp"++show jId++")",
                     "@SP",
                     "M=M+1"
                   ]
  toAsm (Lt jId) = unlines
                   [
                     "@SP",
                     "M=M-1", -- M = &y
                     "A=M",
                     "D=M", -- D = M = y
                     "@SP",
                     "M=M-1",
                     "A=M", -- M = x
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
                     "(endLtJmp"++show jId++")",
                     "@SP",
                     "M=M+1"
                   ]
  toAsm And = unlines
              [
                "@SP",
                "M=M-1",
                "A=M",
                "D=M",
                "@SP",
                "M=M-1",
                "A=M",
                "M=D&M",
                "@SP",
                "M=M+1"
              ]
  toAsm Or = unlines
             [
               "@SP",
               "M=M-1",
               "A=M",
               "D=M",
               "@SP",
               "M=M-1",
               "A=M",
               "M=D|M",
               "@SP",
               "M=M+1"
             ]
  toAsm Not = unlines
              [
                "@SP", -- A = &(&y)
                "M=M-1", -- M = &y
                "A=M", -- A = &y
                "M=!M", -- y = -y
                "@SP",
                "M=M+1"
              ]

main :: IO ()
main = do
  [infile, outfile] <- getArgs
  input <- readFile infile
  let vmCmds = runParser parseVM 0 infile input
  case vmCmds of
   Left err -> print err
   Right c -> writeFile outfile (unlines . map toAsm $ c)
  return ()
