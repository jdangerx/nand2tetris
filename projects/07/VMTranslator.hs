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
                "M=D+M" -- x = y + x
              ]
  toAsm Sub = unlines
              [
                "@SP",
                "A=M",
                "D=M", -- D = M = y
                "@SP",
                "M=M-1",
                "A=M", -- M = x
                "M=M-D" -- x = x - y
              ]
  toAsm Neg = unlines
              [
                "@SP", -- A = &(&y)
                "A=M", -- A = &y
                "M=-M" -- y = -y
              ]
  toAsm Eq = unlines
             [
               "@SP", -- A = &(&y)
               "A=M", -- A = &y
               "D=M", -- D = M = y
               "@THIS", -- THIS = &y
               "M=D",
               "@SP",  -- A = &(&y)
               "M=M-1", -- A = &(&y-1) = &(&x)
               "A=M", -- A = &x
               "D=M", -- D = x
               "@THIS", -- A = &y
               "D=D|M", -- D = x|y
               "@THAT",
               "M=!D", -- THAT = &(!(x|y))
               "@SP", -- A = &&x
               "A=M", -- A= &x
               "D=M", -- D=x
               "@THIS",
               "D=D&M", -- THIS = &(x&y)
               "@THAT", -- A = &(!(x|y))
               "D=D|M", -- D=(!(x|y) | (x&y)) = x == y
               "@SP", -- A = &&x
               "A=M", -- A = &x
               "M=D" -- x = (x == y)
             ]
  toAsm Gt = unlines
             [
               "@SP",
               "A=M",
               "D=M", -- D = M = y
               "@SP",
               "M=M-1",
               "A=M", -- now M = x
               "M=D&M", -- x = x&y
               "D=1",
               "M=D&M"
             ]

main :: IO ()
main = do
  [infile, outfile] <- getArgs
  input <- readFile infile
  let vmCmds = runParser parseVM () infile input
  case vmCmds of
   Left err -> print err
   Right c -> writeFile outfile (unlines . map toAsm $ c)
  return ()
