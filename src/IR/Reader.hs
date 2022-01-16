module IR.Reader where

import Text.Parsec
import Text.Parsec.Char
import IR.Type
import IR.Function
import IR.Instruction

type Parsec' = Parsec String ()

data Variable = Variable { name :: String, varType :: Type }

parseStartFunc :: Parsec' ()
parseStartFunc = string "#start_function" >> return ()

parseEndFunc :: Parsec' ()
parseEndFunc = string "#end_function" >> return ()

parseType :: Parsec' Type
parseType = parseIntType
  <|> parseFloatType
  <|> parseVoidType
  <|> parseArrayType
  where
    parseIntType = string "int" >> return IntType
    parseFloatType = string "float" >> return FloatType
    parseVoidType = string "void" >> return VoidType
    parseArrayType = do
      tp <- parseType
      char '['
      sz <- read <$> many1 digit
      char ']'
      return $ ArrayType (ArraySize sz) tp

parseVariable :: Type -> Parsec' Variable
parseVariable tp = nonArray <|> array
  where
    nonArray = many1 alphaNum >>= \s -> return $ Variable s tp
    array = do
      name <- many1 alphaNum
      char '['
      count <- read <$> many1 digit
      char ']'
      let arrtp = ArrayType (ArraySize count) tp
      return $ Variable name arrtp

parseVariableLists :: Parsec' [Variable]
parseVariableLists = do
  v1 <- i
  newline
  v2 <- f
  return $ v1 ++ v2
  where
    k t = (spaces >> parseVariable t) `sepBy` (char ',')
    i = string "int-list:" >> k IntType
    f = string "float-list:" >> k FloatType

-- TODO: For Luke
parseInstruction :: Parsec' Instruction
parseInstruction = undefined