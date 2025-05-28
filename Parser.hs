module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum, isUpper, isDigit)

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where 
  fmap f mp = do
    x <- mp
    return (f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  af <*> mp = do
    f <- af
    v <- mp
    return (f v)

instance Monad Parser where
  mp >>= f = Parser $ \s ->
    case parse mp s of
      Nothing -> Nothing
      Just (v, r) -> parse (f v) r

  return x = Parser $ \s -> Just (x, s)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Nothing -> p2 input
      result  -> result

-- 2.1. / 3.2.
-- Parser pentru un caracter care satisface o conditie
satisfy :: (Char -> Bool) -> Parser Char
satisfy cond = Parser (satisfyImpl cond)
satisfyImpl :: (Char -> Bool) -> String -> Maybe (Char, String)
satisfyImpl cond input = case input of
                          (x:xs) | cond x -> Just(x, xs)
                          _               -> Nothing

-- Parser pentru un caracter specific
char :: Char -> Parser Char
char c = satisfy(==c)

-- Parser pentru zero sau mai multe spatii albe de la inceputul inputului
whitespaces :: Parser ()
whitespaces = fmap (const()) (many (satisfy isSpace))

-- many (satisfy isSpace) are tipul Parser [Char] - lista de spatii albe, dar eu vreau sa ignor lista si sa returnez ()
-- aplica fmap const() pe rezultatul obtionut si returneaza ()

-- Parser pentru un identificator (sir de caractere alfanumerice urmate de spatii albe)
identifier :: Parser String
identifier =  (some (satisfy isAlphaNum))
-- 

isMacroChar :: Char -> Bool
isMacroChar c = isUpper c || isDigit c

-- Verifica daca un nume este un macro (toate caracterele sunt majuscule sau cifre)
isMacroName :: String -> Bool
isMacroName name = all isMacroChar name

-- Parser pentru variabila lambda sau macro
parseVar :: Parser Lambda
parseVar = do
  name <- identifier
  if isMacroName name
    then return (Macro name)
    else return (Var name)

-- Parser pentru o abstractie lambda (ex: \x. expr)
parseAbs :: Parser Lambda
parseAbs = do
  _ <-  (char '\\')
  var <- some (satisfy isAlpha)
  whitespaces
  _ <- char '.'
  whitespaces
  body <- parseLambdaAux
  return (Abs var body)

-- Parser pentru aplicatia lambda (ex: (e1 e2))
parseApp :: Parser Lambda
parseApp = do
  _ <-  (char '(')
  e1 <- parseLambdaAux
  whitespaces
  e2 <- parseLambdaAux
  whitespaces
  _ <- char ')'
  return (App e1 e2)

-- Parser auxiliar pentru expresii lambda (abs, app sau var)
parseLambdaAux :: Parser Lambda
parseLambdaAux = parseAbs <|> parseApp <|> parseVar

-- Functie care parseaza o expresie lambda din sirul de caractere
parseLambda :: String -> Lambda
parseLambda input = case parse (parseLambdaAux) input of
                      Just (expr, "") -> expr
                      _               -> error "Expresie lambda invalida"

-- 3.3.
-- Creeaza un binding intre ID si functie 
bindingParser :: Parser Line
bindingParser = do
  name <-  (some (satisfy isMacroChar))
  _ <- char '='
  whitespaces
  body <- parseLambdaAux
  return (Binding name body)

-- Functie care ambaleaza intr-un constructor Eval valoarea si o returneaza
evalParser :: Parser Line
evalParser = do
  expr <- parseLambdaAux
  return (Eval expr)

-- Functie care verifica daca o expresie este Macro sau o Lambda
parseLine :: String -> Either String Line
parseLine input = case parse ((bindingParser <|> evalParser)) input of
                    Just (line, "") -> Right line
                    _             -> Left "err"
