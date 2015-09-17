module Main where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String

lpad :: Int -> String -> String
lpad n s = reverse $ take n $ reverse s ++ repeat '0'

class Hackable a where
  toHack :: Hackable a => a -> String

data Command = Addr Int
             | Comp Mnem (Maybe Dest) (Maybe Jump)
             deriving Show

instance Hackable Command where
  toHack (Addr n) = "0" ++ lpad 15 (showIntAtBase 2 intToDigit n "")
  toHack (Comp m d j) = "111" ++ toHack m
                        ++ fromMaybe "000" (toHack <$> d)
                        ++ fromMaybe "000" (toHack <$> j)

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
  jgt :: Bool,
  jeq :: Bool,
  jlt :: Bool
  }
            deriving Show

instance Hackable Jump where
  toHack (Jump gt eq lt) = map (\b -> if b then '1' else '0') [gt, eq, lt]

jumpFromString :: String -> Jump
jumpFromString "JGT" = Jump True False False
jumpFromString "JEQ" = Jump False True False
jumpFromString "JGE" = Jump True True False
jumpFromString "JLT" = Jump False False True
jumpFromString "JNE" = Jump True False True
jumpFromString "JLE" = Jump False True True
jumpFromString "JMP" = Jump True True True
jumpFromString _ = Jump False False False


addr :: Parser Command
addr = char '@'
       *> (many1 digit <?> "literal value")
       >>= pure . Addr . read

comp :: Parser Command
comp = do
  dest <- optionMaybe (try (many (oneOf "ADM") <* char '='))
  mnem <- many1 (oneOf "+-&|01ADM") <?> "mnemonic"
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
  return $ Comp (Mnem mnem) (destFromString <$> dest) (jumpFromString <$> jump)

command :: Parser Command
command = do
  spaces
  com <- try addr <|> try comp
  comment
  return com

comment :: Parser ()
comment = do
  many (oneOf " \t")
  optional (string "//")
  many (noneOf "\n")
  return ()

getCmds :: Parser [Command]
getCmds = command `sepBy` char '\n' <* eof

-- command :: Parser String
-- command = spaces >>

--           (lookAhead (string "//")
--            <|> string "\n"
--            -- since `space` returns a `Char` we need to `Pure` it to
--            -- make it return a `String`
--            <|> pure <$> space
--           )

main :: IO()
main = return ()
