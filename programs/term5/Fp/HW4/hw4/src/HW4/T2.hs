{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace)
import HW4.Types
import HW4.T1 (ExceptState(..))
import Data.Maybe (fromMaybe)



data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P (ES p)) str = case p (0, str) of
  Success (res :# _) -> Success res
  Error e            -> Error e

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P l) (P r) = P $ ES $ \state -> case runES l state of
    Error _            -> runES r state
    Success s@(_ :# _) -> Success s

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \state@(pos, str) -> case str of
  "" -> Success (() :# state)
  _  -> Error (ErrorAtPos pos)

skipBlank :: Parser ()
skipBlank = void $ many (mfilter isSpace pChar)

parseExpr :: String -> Except ParseError Expr
parseExpr = runP exprParser

pNum :: Parser Expr
pNum = do
  skipBlank
  wholePart <- some (mfilter isDigit pChar)         
  decimalPart <- optional $ do                      
    _ <- checkChar '.'
    digits <- some (mfilter isDigit pChar)
    return ('.' : digits)
  let numberStr = wholePart ++ fromMaybe "" decimalPart
  return . Val $ read numberStr


checkChar :: Char -> Parser Char
checkChar c = do
  skipBlank
  x <- pChar
  if x == c
    then return x
    else parseError


pParenExpr :: Parser Expr
pParenExpr = do
  skipBlank
  _ <- checkChar '('
  expr <- pExpr
  skipBlank
  _ <- checkChar ')'
  return expr

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do
                skipBlank
                f <- op
                skipBlank
                y <- p
                rest (f x y))
             <|> return x 

pExpr :: Parser Expr
pExpr = chainl1 pTerm addOp

pTerm :: Parser Expr
pTerm = chainl1 pFactor mulOp

pFactor :: Parser Expr
pFactor = pNum <|> pParenExpr

addOp :: Parser (Expr -> Expr -> Expr)
addOp = ((\x y -> Op (Add x y)) <$ checkChar '+')
    <|> ((\x y ->Op (Sub x y)) <$ checkChar '-')

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = ((\x y -> Op (Mul x y))  <$ checkChar '*')
    <|> ((\x y -> Op (Div x y)) <$ checkChar '/')

exprParser :: Parser Expr
exprParser = do
  expr <- pExpr
  skipBlank
  pEof
  return expr
