module Main where

import VMParse

import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import Text.Parsec

decSP :: [String]
decSP = ["@SP", "M=M-1", "A=M"]

incSP :: [String]
incSP = ["@SP", "M=M+1"]

pushDSP :: [String]
pushDSP = ["@SP", "A=M", "M=D"] ++ incSP

pushSeg :: Segment -> [String]
pushSeg (Constant (Just n)) =
  ["@" ++ show n, "D=A"]
pushSeg (Local (Just n)) =
  ["@LCL", "D=M", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (Argument (Just n)) =
  ["@ARG", "D=M", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (This (Just n)) =
  ["@THIS", "D=M", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (That (Just n)) =
  ["@THAT", "D=M", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (Temp (Just n)) =
  ["@5", "D=A", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (Pointer (Just n)) =
  ["@THIS", "D=A", "@" ++ show n, "A=D+A", "D=M"]
pushSeg (Static filename (Just n)) =
  ["@" ++ filename ++ "." ++ show n, "D=M"]
pushSeg _ = []

popToR13 :: [String]
popToR13 = ["@SP", "M=M-1", "A=M", "D=M", "@R13", "M=D"] -- R13 = **SP

putR13IntoR14 :: [String]
putR13IntoR14 = ["@R13", "D=M", "@R14", "A=M", "M=D"]

popSeg :: Segment -> [String]
popSeg (Local (Just n)) =
  ["@LCL", "D=M", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (Argument (Just n)) =
  ["@ARG", "D=M", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (This (Just n)) =
  ["@THIS", "D=M", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (That (Just n)) =
  ["@THAT", "D=M", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (Temp (Just n)) =
  ["@5", "D=A", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (Pointer (Just n)) =
  ["@THIS", "D=A", "@" ++ show n, "D=D+A", "@R14", "M=D"]
popSeg (Static filename (Just n)) =
  ["@" ++ filename ++ "." ++ show n, "D=A", "@R14", "M=D"]
popSeg _ = []

class Assemblable a where
  toAsm :: a -> [String]

instance Assemblable Command where
  toAsm (Call func nargs jId) =
    let returnId = func ++"_return_" ++ show jId
    in
     [ "@" ++ returnId, "D=A" ] ++ pushDSP ++  -- push return address
     [ "@LCL", "D=M" ] ++ pushDSP ++  -- push LCL
     [ "@ARG", "D=M" ] ++ pushDSP ++  -- push ARG
     [ "@THIS", "D=M" ] ++ pushDSP ++  -- push THIS
     [ "@THAT", "D=M" ] ++ pushDSP ++  -- push THAT
     [ "@SP" -- A = &SP
     , "D=M"  -- D = SP
     , "@" ++ show nargs
     , "D=D-A"  -- D = SP - nargs
     , "@5"
     , "D=D-A"  -- D = SP - nargs - 5
     , "@ARG"
     , "M=D"  -- ARG = SP - nargs - 5
     , "@SP" -- A = &SP
     , "D=M"  -- D = SP
     , "@LCL"  -- A = &LCL
     , "M=D"  -- LCL = SP
     , "@"++func
     , "0;JMP"
     , "(" ++ returnId ++ ")" ]
  toAsm Return =
    [ "@LCL" -- A = &LCL
    , "D=M"  -- D = LCL
    , "@R13"
    , "M=D" -- R13 = LCL, aka the beginning of the frame
    , "@5"
    , "D=D-A" -- D = LCL - 5 == &RET
    , "A=D"
    , "D=M" -- D = RET
    , "@R14"
    , "M=D" -- R14 = returnAddress
    , "@SP"
    , "M=M-1"
    , "A=M"
    , "D=M" -- D = tip
    , "@ARG" -- A = &ARG
    , "A=M"  -- A = ARG
    , "M=D"  -- *ARG = tip (this will be the new tip of the stack after return)
    , "D=A" -- D = ARG
    , "@SP"
    , "M=D+1" -- SP = ARG + 1

    , "@R13"
    , "D=M-1" -- D = LCL - 1
    , "A=D"
    , "D=M" -- D = *(LCL-1)
    , "@THAT"
    , "M=D" -- *THAT = *(LCL - 1)

    , "@R13"
    , "D=M" -- D = LCL
    , "@2"
    , "D=D-A" -- D = LCL - 2
    , "A=D"
    , "D=M" -- D = *(LCL-2)
    , "@THIS"
    , "M=D" -- *THIS = *(LCL - 2)

    , "@R13"
    , "D=M" -- D = LCL
    , "@3"
    , "D=D-A" -- D = LCL - 3
    , "A=D"
    , "D=M" -- D = *(LCL-3)
    , "@ARG"
    , "M=D" -- *ARG = *(LCL - 3)

    , "@R13"
    , "D=M" -- D = LCL
    , "@4"
    , "D=D-A" -- D = LCL - 4
    , "A=D"
    , "D=M" -- D = *(LCL-4)
    , "@LCL"
    , "M=D" -- *LCL = *(LCL - 4)

    , "@R14" -- A = RET
    , "A=M"
    , "0;JMP"]
  toAsm (Function func nargs) =
    ["(" ++ func ++ ")"] ++
    concat (replicate nargs (toAsm $ Push (Local (Just 0))))
  toAsm (Label lname fname) = ["(" ++ fname ++ "." ++ lname ++")"]
  toAsm (Goto lname fname) = ["@" ++ fname ++ "." ++ lname, "0;JMP"]
  toAsm (IfGoto lname fname) =
    decSP ++
    [
      "D=M",
      "@" ++ fname ++ "." ++ lname,
      "D;JNE"
    ]
  toAsm (Push seg) =
    pushSeg seg ++ pushDSP
  toAsm (Pop seg) =
    popToR13 ++ popSeg seg ++ putR13IntoR14
  toAsm Add =
    decSP ++ "D=M" : decSP ++ "M=D+M" : incSP
  toAsm Sub =
    decSP ++ "D=M" : decSP ++ "M=M-D" : incSP
  toAsm Neg = decSP ++ "M=-M" : incSP
  toAsm (Eq fn jId) =
    decSP ++
    "D=M" : -- D = M = y
    decSP ++
    [
      "D=M-D", -- D = x - y
      "@" ++ fn ++ "._eqJmp_"++show jId,
      "D;JEQ", -- x = x - y
      "@SP",
      "A=M",
      "M=0",
      "@" ++ fn ++ "._endEqJmp_"++show jId,
      "0;JMP",
      "(" ++ fn ++ "._eqJmp_"++show jId++")",
      "@SP",
      "A=M",
      "M=-1",
      "(" ++ fn ++ "._endEqJmp_"++show jId++")"
    ] ++
    incSP
  toAsm (Gt fn jId) =
    decSP ++
    "D=M" : -- D = M = y
    decSP ++
    [
      "D=M-D", -- D = x - y
      "@" ++ fn ++ "._gtJmp_"++show jId,
      "D;JGT", -- x = x - y
      "@SP",
      "A=M",
      "M=0",
      "@" ++ fn ++ "._endGtJmp_"++show jId,
      "0;JMP",
      "(" ++ fn ++ "._gtJmp_"++show jId++")",
      "@SP",
      "A=M",
      "M=-1",
      "(" ++ fn ++ "._endGtJmp_"++show jId++")"
    ] ++
    incSP
  toAsm (Lt fn jId) =
    decSP ++
    "D=M" : -- D = M = y
    decSP ++
    [
      "D=M-D", -- D = x - y
      "@" ++ fn ++ "._ltJmp_"++show jId,
      "D;JLT", -- x = x - y
      "@SP",
      "A=M",
      "M=0",
      "@" ++ fn ++ "._endLtJmp_"++show jId,
      "0;JMP",
      "(" ++ fn ++ "._ltJmp_"++show jId++")",
      "@SP",
      "A=M",
      "M=-1",
      "(" ++ fn ++ "._endLtJmp_"++show jId++")"
    ] ++
    incSP
  toAsm And =
    decSP ++ "D=M" : decSP ++ "M=D&M" : incSP
  toAsm Or =
    decSP ++ "D=M" : decSP ++ "M=D|M" : incSP
  toAsm Not = decSP ++ "M=!M" : incSP

parseFile :: FilePath -> IO (Either ParseError [Command])
parseFile infile =
  liftM
  (runParser parseVM (takeBaseName infile, 1) infile)
  (readFile infile)

initCall :: [String]
initCall = [ "@256"
           , "D=A"
           , "@SP"
           , "M=D"  -- SP = 256
           , "@LCL"
           , "M=D"  -- LCL = 256
           , "@251"
           , "D=A"
           , "@ARG"
           , "M=D"  -- ARG = 251
           , "@3000"
           , "D=A"
           , "@THIS"
           , "M=D"  -- THIS = 3000
           , "@4000"
           , "D=A"
           , "@THAT"
           , "M=D"  -- THAT = 3000
           ]
           ++ toAsm (Call "Sys.init" 0 0)

main :: IO ()
main = do
  [infile] <- getArgs
  isFile <- doesFileExist infile
  isDir <- doesDirectoryExist infile
  infiles <- if isFile
            then return [infile]
            else if isDir
                 then map (combine infile) . filter ((== ".vm") . takeExtension)
                      <$> getDirectoryContents infile
                 else return []
  outfile <-
    let
      asm = (takeBaseName . dropTrailingPathSeparator $ infile) -<.> "asm"
    in
     if isFile
     then return $ combine (takeDirectory infile) asm
             else if isDir
                  then return $ combine infile asm
                  else return ""
  vmCmds <- (concat <$>) <$> (sequence <$> mapM parseFile infiles)
  case vmCmds of
   Left err -> print err
   Right c -> writeFile outfile (unlines . (initCall ++) . concatMap toAsm $ c)
  return ()
