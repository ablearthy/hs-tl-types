{-# LANGUAGE OverloadedStrings #-}
module Telegram.TL.Parser (
  VarIdent (..), VarIdentFull (..), TypeIdent (..),
  TypeExpr (..), OptArg (..), Arg (..), ConditionalDef (..),
  Combinator (..), Schema (..), schemaP
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Void
import Data.Word
import Data.List (foldl')
import Data.Char (digitToInt)
import Data.Bits
import Data.Maybe (fromMaybe)

import System.IO.Unsafe

type Parser = Parsec Void T.Text


tripleMinus :: Parser ()
tripleMinus = () <$ string "---"

sc :: Parser ()
sc = L.space
  (() <$ some (char ' ' <|> char '\t' <|> char '\n' <|> char '\r'))
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


cBetween :: Char -> Char -> Parser a -> Parser a
cBetween s e p = between (lexeme (char s)) (lexeme (char e)) p


funcHeader :: Parser ()
funcHeader = () <$ between (lexeme tripleMinus) (lexeme tripleMinus) (lexeme (string "functions"))

typesHeader :: Parser ()
typesHeader = () <$ between (lexeme tripleMinus) (lexeme tripleMinus) (lexeme (string "types"))


data VarIdent = VarIdent (Maybe T.Text) T.Text deriving Show
data VarIdentFull = VarIdentFull VarIdent (Maybe Word32) deriving Show
data TypeIdent = TypeIdent T.Text | TypeHash deriving Show


data TypeExpr 
  = TypeExpr TypeExpr
  | TypeBare TypeExpr
  | Type TypeIdent
  | TypeAp TypeExpr TypeExpr
  deriving Show

data Combinator = Combinator VarIdentFull [OptArg] [Arg] TypeExpr deriving Show

data OptArg = OptArg T.Text Bool TypeExpr deriving Show

data Arg
  = ConditionalArg (Maybe T.Text) ConditionalDef Bool TypeExpr
  | ArrayArg (Maybe T.Text) (Maybe TypeExpr) [Arg]
  | Arg (Maybe T.Text) Bool TypeExpr
  deriving Show

data ConditionalDef = ConditionalDef T.Text (Maybe Int) deriving Show


identChar :: Parser Char
identChar = letterChar <|> digitChar <|> char '_'

varIdentP' :: Parser T.Text
varIdentP' = T.pack <$> ((:) <$> letterChar <*> many identChar)

varIdentOpt :: Parser (Maybe T.Text)
varIdentOpt = (Nothing <$ char '_') <|> (Just <$> varIdentP')

varIdentP :: Parser VarIdent
varIdentP = do
  ns <- optional . try $ do
    first <- lowerChar
    rest <- many identChar
    char '.'
    pure $ T.pack (first:rest)
  v <- varIdentP'
  pure (VarIdent ns v)



varIdentFullP :: Parser VarIdentFull
varIdentFullP = VarIdentFull <$> varIdentP <*> optional hashP
  where
    hashP :: Parser Word32
    hashP = do
      char '#'
      L.hexadecimal

termP = bareP <|> tExprP <|> apP
  where
  bareP = TypeBare <$> (char '%' *> typeExprP)
  tExprP = TypeExpr <$> between (lexeme (char '(')) (lexeme (char ')')) typeExprP
  apP = do
    ti <- Type <$> lexeme typeIdentP
    aps <- optional $ parens $ do
      second <- typeExprP 
      case second of
        TypeExpr x -> parensP $ TypeAp ti (TypeExpr x)
        x -> parensP $ TypeAp ti (TypeExpr x)
    pure $ fromMaybe ti aps

  parens = between (lexeme (char '<')) (lexeme (char '>'))

  parensP :: TypeExpr -> Parser TypeExpr
  parensP te = do
    another <- optional $ (lexeme (char ',')) *> (lexeme typeExprP)
    case another of
      Nothing -> pure $ TypeExpr te
      Just (TypeExpr x) -> parensP $ TypeAp te (TypeExpr x)
      Just x -> parensP $ TypeAp te (TypeExpr x)

  typeIdentP :: Parser TypeIdent
  typeIdentP = (TypeHash <$ (char '#')) <|> (TypeIdent <$> varIdentP')

typeExprP :: Parser TypeExpr 
typeExprP = lexeme termP >>= exprP
  where
    exprP :: TypeExpr -> Parser TypeExpr
    exprP te = do
      another <- optional (try (lexeme termP))
      case another of
        Nothing -> pure te
        Just x -> exprP (TypeAp te x)



optArgsP :: Parser [OptArg]
optArgsP = between (lexeme (char '{')) (lexeme (char '}')) $ do
  vars <- some (lexeme varIdentP')
  lexeme (char ':')
  exclOpt <- optional . lexeme $ char '!'
  let excl = maybe False (const True) exclOpt
  typ <- lexeme typeExprP
  pure $ map (\x -> OptArg x excl typ) vars

      
argsP :: Parser [Arg]
argsP = parensP <|> try arrayP <|> try conditionalP <|> anonymousP
  where
    parensP :: Parser [Arg]
    parensP = cBetween '(' ')' $ do
      vars <- some (lexeme varIdentOpt)
      lexeme (char ':')
      exclOpt <- optional . lexeme $ char '!'
      let excl = maybe False (const True) exclOpt
      typ <- lexeme termP 
      pure $ map (\x -> Arg x excl typ) vars

    conditionalP :: Parser [Arg]
    conditionalP = do
      var <- lexeme varIdentOpt
      lexeme (char ':')
      def <- optional $ try $ do
        cVar <- varIdentP'
        cInt <- optional $ do
          lexeme (char '.')
          lexeme (L.decimal)
        lexeme (char '?')
        pure $ ConditionalDef cVar cInt

      exclOpt <- optional . lexeme $ char '!'
      let excl = maybe False (const True) exclOpt
      typ <- lexeme termP 
      case def of
        Nothing -> pure [Arg var excl typ]
        Just x -> pure [ConditionalArg var x excl typ]


    arrayP :: Parser [Arg]
    arrayP = do
      var <- optional . try $ (lexeme varIdentOpt) <* (lexeme (char ':'))
      mul <- optional . try $ (lexeme typeExprP) <* (lexeme (char '*'))
      args <- cBetween '[' ']' argsP
      pure [ArrayArg (relax var) mul args]
      where
        relax :: Maybe (Maybe T.Text) -> Maybe T.Text
        relax (Just x) = x
        relax Nothing = Nothing
        

    anonymousP :: Parser [Arg]
    anonymousP = do
      exclOpt <- optional . lexeme $ char '!'
      let excl = maybe False (const True) exclOpt
      typ <- lexeme termP 
      pure [Arg Nothing excl typ]

combinatorP :: Parser Combinator
combinatorP = do
  vif <- lexeme varIdentFullP
  optArgs <- concat <$> many (lexeme optArgsP)
  args <- concat <$> many (lexeme argsP)
  lexeme (char '=')
  typ <- lexeme typeExprP
  lexeme (char ';')
  pure $ Combinator vif optArgs args typ

data Schema = Schema [Combinator] [Combinator] deriving Show

schemaP :: Parser Schema
schemaP = do
  constrs <- many (lexeme combinatorP)
  more <- many $ do
    (Left <$> ((lexeme typesHeader) *> many (lexeme combinatorP))) <|> (Right <$> ((lexeme funcHeader) *> many (lexeme combinatorP)))
  pure $ foldl' f (Schema constrs []) more
  where
    f :: Schema -> Either [Combinator] [Combinator] -> Schema
    f (Schema typs funcs) (Left x) = Schema (typs ++ x) funcs
    f (Schema typs funcs) (Right x) = Schema typs (funcs ++ x)
