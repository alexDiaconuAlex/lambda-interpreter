module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.

parseExpr ('(':xs) acc level = parseExpr xs ('(' : acc) (level + 1)
parseExpr (')':xs) acc level = parseExpr xs (')' : acc) (level - 1)
parseExpr (' ':xs) acc 0 = (reverse acc) : parseExpr xs "" 0
parseExpr (x:xs) acc level = parseExpr xs (x : acc) level
parseExpr [] acc _ = [reverse acc]

-- a b c

-- c b a

getToken "" = ""
getToken ('.':xs) = xs
getToken (_:xs) = getToken xs

parseLambda :: String -> Lambda
parseLambda s = if ((==) (s !! 0) '(') then (App e1 e2) --                    ii dau numele fara \ de la inceput
                else if ((==) (s !! 0) '\\') then (Abs (takeWhile (/= '.') (tail s)) (parseLambda (getToken s)))
                else if (notElem ' ' s) then (Var s)
                else undefined
                where
                    tokens = parseExpr (tail (init s)) "" 0 -- ( | rest | )
                    e1 = parseLambda (tokens !! 0)
                    e2 = parseLambda (tokens !! 1)

-- 3.3.
parseLine :: String -> Either String Line
parseLine = undefined
