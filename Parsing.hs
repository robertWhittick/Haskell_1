{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}

module Parsing where

import World
import Actions

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 |||

{-
The monad of parsers
--------------------
-}

newtype Parser a              =  P (String -> [(a,String)])

data Operation =
   Go Direction |
   Get Object |
   Drop Object |
   Examine Object |
   Pour Object |
   Drink Object |
   Open Object |
   Inv |
   Quit |
   Error

operations :: GameData -> Operation -> (GameData, String)
operations state cmd = case cmd of
                        Go x -> go x state
                        Get x -> get x state
                        Drop x -> put x state
                        Examine x -> examine x state
                        Pour x -> pour x state
                        Drink x -> drink x state
                        Open x -> open x state
                        Inv -> inv state
                        Quit -> quit state
                        _ -> (state, "I don't understand")

operationParser :: String -> Operation
operationParser = \inp -> case inp of
     'g':('o':(' ':ys)) -> do
        maybe Error Go (directions ys)
     'g':('e':('t':(' ':ys))) -> do
        maybe Error Get (object ys)
     'd':('r':('o':('p':(' ':ys)))) -> do
        maybe Error Drop (object ys)
     'e':('x':('a':('m':('i':('n':('e':(' ':ys))))))) -> do
        maybe Error Examine (object ys)
     'p':('o':('u':('r':(' ':ys)))) -> do
        maybe Error Pour (object ys)
     'd':('r':('i':('n':('k':(' ':ys))))) -> do
        maybe Error Drink (object ys)
     'o':('p':('e':('n':(' ':ys)))) -> do
        maybe Error Open (object ys)
     "inv" -> Inv
     "quit" -> Quit
     [] -> Error
     ys -> Error

instance Functor Parser where
   fmap f p = do p' <- p
                 return (f p')

instance Applicative Parser where
   pure = return
   f <*> a = do f' <- f
                a' <- a
                return (f' a')

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Alternative Parser where
   empty = mzero
   p <|> q = p ||| q

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Choice
------
-}

(|||)                         :: Parser a -> Parser a -> Parser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p ||| return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  ||| nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()
{-
Ignoring spacing
----------------
-}

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)
