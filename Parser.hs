module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum, isUpper, isDigit)

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    return (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  Parser pf <*> Parser pa = Parser $ \input -> do
    (f, rest1) <- pf input
    (a, rest2) <- pa rest1
    return (f a, rest2)

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \input -> do
    (x, rest) <- p input
    parse (f x) rest

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
-- implementare parser care verifica daca primul caracter din input satisface conditia cond
-- daca da, returneaza caracterul si restul inputului
-- daca nu, returneaza Nothing
satisfyImpl :: (Char -> Bool) -> String -> Maybe (Char, String)
satisfyImpl cond input = case input of
                          (c:cs) | cond c -> Just (c, cs)
                          _               -> Nothing

-- Parser pentru un caracter specific
char :: Char -> Parser Char
char c = satisfy(eqChar c)
-- foloseste satisfy pentru a construi un parser care accepta exact caracterul c

eqChar :: Char -> Char -> Bool
eqChar expected actual = expected == actual

-- Parser pentru zero sau mai multe spatii albe
whitespaces :: Parser ()
whitespaces = fmap (const ()) (many (satisfy isSpace))
-- foloseste combinatorul many cu satisfy isSpace pentru a consuma toate spatiile din input
-- returneaza unit () dupa consumarea spatiilor

-- Parser pentru un sir de caractere
string :: String -> Parser String
string s = traverse char s
-- foloseste traverse cu parserul char pentru a parsa o secventa exacta de caractere

-- Primeste un parser p :: Parser a si returneaza un nou parser 
token :: Parser a -> Parser a
token p = do
  whitespaces
  x <- p
  whitespaces
  return x
-- 1. ignora spatiile alge din fata
-- 2. ruleaza parserul p
-- 3. ignora spatiile albe de dupa 
-- 4. returneaza rezulatul

-- Parser pentru un identificator (sir de caractere alfanumerice urmate de spatii albe)
identifier :: Parser String
identifier = token (some (satisfy isAlphaNum))
-- foloseste some cu satisfy isAlphaNum pentru a citi un sir non-vid de caractere alfanumerice
-- dupa identificator, consuma spatii albe folosind whitespaces

isMacroChar :: Char -> Bool
isMacroChar c = isUpper c || isDigit c

-- Verifica daca un nume este un macro (toate caracterele sunt majuscule sau cifre)
isMacroName :: String -> Bool
isMacroName name = not (null name) && all isMacroChar name
-- verifica daca numele nu e vid si fiecare caracter este majuscula sau cifra

-- Parser pentru variabila lambda sau macro
parseVar :: Parser Lambda
parseVar = do
  name <- identifier
  if isMacroName name
    then return (Macro name)
    else return (Var name)
-- parseaza un identificator cu identifier
-- daca e macro (isMacroName), returneaza Macro
-- altfel returneaza Var

-- Parser pentru o abstractie lambda (ex: \x. expr)
parseAbs :: Parser Lambda
parseAbs = do
  _ <- token (char '\\')
  var <- some (satisfy isAlpha)
  whitespaces
  _ <- char '.'
  whitespaces
  body <- parseLambdaAux
  return (Abs var body)
-- 1. parseaza caracterul '\' folosind char si <|>
-- 2. consuma spatii albe (le parseaza si le ignora)
-- 3. parseaza numele variabilei (un sir de caractere alfabetice)
-- 4. consuma spatii albe
-- 5. parseaza punctul '.'
-- 6. consuma spatii albe
-- 7. parseaza corpul lambda cu parseLambdaAux
-- 8. returneaza Abs cu variabila si corpul

-- Parser pentru aplicatia lambda (ex: (e1 e2))
parseApp :: Parser Lambda
-- 1. parseaza caracterul '('
-- 2. consuma spatii albe
-- 3. parseaza expresia e1 cu parseLambdaAux
-- 4. consuma spatii albe
-- 5. parseaza expresia e2 cu parseLambdaAux
-- 6. consuma spatii albe
-- 7. parseaza caracterul ')'
-- 8. returneaza App cu e1 si e2
parseApp = do
  _ <- token (char '(')
  e1 <- parseLambdaAux
  whitespaces
  e2 <- parseLambdaAux
  whitespaces
  _ <- char ')'
  return (App e1 e2)

-- Parser auxiliar pentru expresii lambda (abs, app sau var)
parseLambdaAux :: Parser Lambda
parseLambdaAux = parseAbs <|> parseApp <|> parseVar
-- incearca sa parsezi o abstractie, apoi o aplicatie, apoi o variabila
-- in aceasta ordine, folosind <|> pentru a incerca alternativ

-- Functie care parseaza o expresie lambda din sirul de caractere
parseLambda :: String -> Lambda
parseLambda input = case parse (token parseLambdaAux) input of
                      Just (expr, "") -> expr
                      _               -> error "Expresie lambda invalida"
-- 1. parseaza inputul folosind parserul compus: consuma spatii la inceput si la final
-- 2. daca parse-ul reuseste si inputul e consumat complet, returneaza rezultatul
-- 3. altfel, arunca eroare "Expresie lambda invalida"
----------------------------------------------------------------------------------------------

-- 3.3.
-- Functie care verifica daca un caracter este Macro sau cifra
isBindingChar :: Char -> Bool
isBindingChar c = isUpper c || isDigit c

-- Creeaza un binding intre ID si functie 
bindingParser :: Parser Line
bindingParser = do
  name <- token (some (satisfy isBindingChar))
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
parseLine input = case parse (token (bindingParser <|> evalParser)) input of
                    Just (ln, "") -> Right ln
                    _             -> Left "err"