module Main where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe (catMaybes, fromMaybe)
import System.Environment (getArgs)
import Text.Parsec
import qualified Data.Map as M

lpad :: Int -> String -> String
lpad n s = reverse $ take n $ reverse s ++ repeat '0'

class Hackable a where
  toHack :: Hackable a => a -> String

data Command = Addr (Either String Int)
             | Comp Mnem (Maybe Dest) (Maybe Jump)
             | Label {name :: String, labelAddr :: Int}
             deriving Show

isLabel :: Command -> Bool
isLabel (Label _ _) = True
isLabel _ = False

instance Hackable Command where
  toHack (Addr (Right n)) = "0" ++ lpad 15 (showIntAtBase 2 intToDigit n "")
  toHack (Comp m d j) = "111" ++ toHack m
                        ++ fromMaybe "000" (toHack <$> d)
                        ++ fromMaybe "000" (toHack <$> j)
  toHack (Addr (Left s)) = ""
  toHack (Label _ _) = ""

data Mnem = Mnem String
          deriving Show

instance Hackable Mnem where
  toHack (Mnem s) =
    let
      repl 'M' = 'O'
      repl 'A' = 'O'
      repl c = c
      mFlag = if 'M' `elem` s then '1' else '0'
    in
     case map repl s of
      "0" -> mFlag : "101010"
      "1" -> mFlag : "111111"
      "-1" -> mFlag : "111010"
      "D" -> mFlag : "001100"
      "O" -> mFlag : "110000"
      "!D" -> mFlag : "001101"
      "!O" -> mFlag : "110001"
      "-D" -> mFlag : "001111"
      "-O" -> mFlag : "110011"
      "D+1" -> mFlag : "011111"
      "O+1" -> mFlag : "110111"
      "D-1" -> mFlag : "001110"
      "O-1" -> mFlag : "110010"
      "D+O" -> mFlag : "000010"
      "D-O" -> mFlag : "010011"
      "O-D" -> mFlag : "000111"
      "D&O" -> mFlag : "000000"
      "D|O" -> mFlag : "010101"

data Dest = Dest {
  destA :: Bool,
  destD :: Bool,
  destM :: Bool
  }
            deriving Show

instance Hackable Dest where
  toHack (Dest a d m) = map (\b -> if b then '1' else '0') [a, d, m]

destFromString :: String -> Dest
destFromString s =
  Dest {
    destA = 'A' `elem` s,
    destD = 'D' `elem` s,
    destM = 'M' `elem` s
  }

data Jump = Jump {
  jlt :: Bool,
  jeq :: Bool,
  jgt :: Bool
  }
            deriving Show

instance Hackable Jump where
  toHack (Jump lt eq gt) = map (\b -> if b then '1' else '0') [lt, eq, gt]

jumpFromString :: String -> Jump
jumpFromString "JGT" = Jump False False True
jumpFromString "JEQ" = Jump False True False
jumpFromString "JGE" = Jump False True True
jumpFromString "JLT" = Jump True False False
jumpFromString "JNE" = Jump True False True
jumpFromString "JLE" = Jump True True False
jumpFromString "JMP" = Jump True True True
jumpFromString _ = Jump False False False


addr :: Parsec String (M.Map String Int, Int) Command
addr =
  do
    char '@'
    rawVal <- (try (many1 digit) <?> "literal value")
              <|> (try letter
                   >>= (\initC -> many (alphaNum <|> oneOf "_.$:")
                                 >>= (\rest -> pure $ initC : rest)))
    (st, _) <- getState
    let
      val = case reads rawVal :: [(Int, String)] of
        [(a, "")] -> Right a
        [] -> (\m -> case m of
                    Just a -> Right a
                    Nothing -> Left rawVal) . M.lookup rawVal $ st
    modifyState ((+ 1) <$>)
    return $ Addr val

getOrCreateVariable :: String -> M.Map String Int -> (M.Map String Int, Int)
getOrCreateVariable v st =
  let newAddr = max 16 ((+ 1) . maximum . (0:) . map snd . M.toList $ st)
  in
   case M.lookup v st of
    Nothing -> (M.insertWith (\_ old -> old) v newAddr st, newAddr)
    Just address -> (st, address)


asmLabel :: Parsec String (M.Map String Int, Int) Command
asmLabel =
  do
    name <- between (char '(') (char ')') (many (alphaNum <|> oneOf "_.$:"))
    (_, cmdsSeen) <- getState
    modifyState (\(st, cs) -> (M.insert name cs st, cs))
    return $ Label name cmdsSeen

comp :: Parsec String (M.Map String Int, Int) Command
comp = do
  dest <- optionMaybe (try (many (oneOf "ADM") <* char '='))
  mnem <- many1 (oneOf "+-&|!01ADM") <?> "mnemonic"
  jump <- optionMaybe (char ';' *>
                      (try (string "JGT")
                       <|> try (string "JEQ")
                       <|> try (string "JGE")
                       <|> try (string "JLT")
                       <|> try (string "JNE")
                       <|> try (string "JLE")
                       <|> try (string "JMP")
                      )
                     )
  modifyState ((+ 1) <$>)
  return $ Comp (Mnem mnem) (destFromString <$> dest) (jumpFromString <$> jump)

command :: Parsec String (M.Map String Int, Int) (Maybe Command)
command = do
  spaces
  com <- optionMaybe (try addr <|> try comp <|> try asmLabel)
  comment
  return com

comment :: Parsec String a ()
comment = do
  many (oneOf " \t")
  optional (string "//")
  many (noneOf "\n")
  return ()

getCmds :: Parsec String (M.Map String Int, Int) [Command]
getCmds = do
  maybeCmds <- command `sepBy` char '\n' <* spaces <* eof
  (st, _) <- getState
  let cmds = resolveVariables (catMaybes maybeCmds) st
  return cmds

getMCmds :: Parsec String (M.Map String Int, Int) [Maybe Command]
getMCmds = command `sepBy` char '\n' <* spaces <* eof

resolveVariables :: [Command] -> M.Map String Int -> [Command]
resolveVariables cmds symbolTable =
  fst $ foldr (\cmd (ls, st) ->
              case cmd of
               Addr (Left _) -> resolveVar cmd ls st
               _ -> (cmd:ls, st)
            ) ([], symbolTable) cmds

resolveVar ::
  Command -> [Command] -> M.Map String Int -> ([Command], M.Map String Int)
resolveVar (Addr (Left varName)) ls st =
  case M.lookup varName st of
   Just address -> (Addr (Right address):ls, st)
   Nothing -> let address = getSmallestVarAddr st 16
             in
              (Addr (Right address):ls, M.insert varName address st)

getSmallestVarAddr :: M.Map String Int -> Int -> Int
getSmallestVarAddr st n = if n `elem` (map snd . M.toList $ st)
                          then getSmallestVarAddr st (n + 1)
                          else n

testParse :: IO ()
testParse =
   do
     input <- readFile "test.asm"
     let cmds = runParser getCmds (M.empty, 0) "test.asm" input
     print cmds
     case unlines . map toHack . filter (not . isLabel) <$> cmds of
      Left err -> print err
      Right c -> writeFile "testHaskell.hack" c

initialST :: M.Map String Int
initialST = M.fromList
            [
              ("SP", 0),
              ("LCL", 1),
              ("ARG", 2),
              ("THIS", 3),
              ("THAT", 4),
              ("R0", 0),
              ("R1", 1),
              ("R2", 2),
              ("R3", 3),
              ("R4", 4),
              ("R5", 5),
              ("R6", 6),
              ("R7", 7),
              ("R8", 8),
              ("R9", 9),
              ("R10", 10),
              ("R11", 11),
              ("R12", 12),
              ("R13", 13),
              ("R14", 14),
              ("R15", 15),
              ("SCREEN", 16384),
              ("KBD", 24576)
            ]

main :: IO()
main = do
  [infile, outfile] <- getArgs
  input <- readFile infile
  let cmds = runParser getCmds (initialST, 0) infile input
  print $ filter (\l -> name l == "ponggame.0") . filter isLabel <$> cmds
  case unlines . map toHack . filter (not . isLabel) <$> cmds of
   Left err -> print err
   Right c -> writeFile outfile c
  return ()

-- need to make the symbol table in two passes:
-- build up labels when parsing
-- then go through all addr's and try to resolve them.
