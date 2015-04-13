---
layout: post
title:  "Simple Parser"
date:   2015-03-03 23:46:06
categories: syntax, languages, programming
---

{% highlight haskell %}
import Data.Char (isSpace)

-- A simple interpreter for a math expressions.
-- Supported binary operations: +-/*
-- Supported unary operations: +-
-- All operators are right associative.
-- Operator precedence: Unary Operators > 
--                      Multiplication, Division > 
--                      Addition, Subtraction
-- Output is modulo 10^9 + 7

--------------------------------------------------------------------------------
--  Grammar

data Exp = 
    Exp1 Term OpE Exp
  | Exp2 Term

data Term = 
    Trm1 Factor OpT Term
  | Trm2 Factor

data Factor = 
    Number Integer
  | Fac2 UnOp Factor
  | Fac3 Exp

data UnOp = 
    NotNeg 
  | Neg

data OpE = 
    Sub
  | Add

data OpT = 
    Div 
  | Mul


--------------------------------------------------------------------------------
--  Parser Tools

data Parser t a = Parser ([t] -> Either String (a,[t]))

instance Monad (Parser s) where
    return x = Parser (\xs -> Right (x,xs))
    Parser h >>= f = Parser (\s -> case h s of
                                    Left a -> Left a
                                    Right (x,s') -> case f x of
                                                     (Parser m) -> m s')

(<|>)                      :: Parser s a -> Parser s a -> Parser s a
(Parser f1) <|> (Parser f2) =  Parser (\s -> case f1 s of
                                            Left _ -> f2 s
                                            a -> a)

parse           :: Parser t a -> [t] -> Either String (a, [t])
parse (Parser f) = f

sat  :: (Char -> Bool) -> Parser Char Char
sat p = Parser f
  where f (x:xs) | p x = Right (x,xs)
        f xs = Left $ "Can't parse sat at " ++ xs

many  :: Show a => Parser c a -> Parser c [a]
many p = lots <|> none
  where lots = do { x <- p; xs <- many p; return (x:xs)}
        none = return []

whitespace :: Parser Char Char
whitespace  = sat isSpace

lexeme  :: Parser Char b -> Parser Char b
lexeme p = do 
    x <- p 
    many whitespace
    return x

parens  :: Parser Char b -> Parser Char b
parens p = do 
    lexeme $ sat (=='(')
    x <- p 
    lexeme $ sat (==')')
    return x


--------------------------------------------------------------------------------
--  Parser

parseExp :: Parser Char Exp
parseExp = parseExp1 
       <|> parseExp2

parseExp1 :: Parser Char Exp
parseExp1  = do
    t <- parseTerm
    o <- parseOpE
    e <- parseExp
    many whitespace
    return $ Exp1 t o e

parseExp2 :: Parser Char Exp
parseExp2  = do 
    t <- parseTerm
    many whitespace
    return $ Exp2 t

parseTerm :: Parser Char Term
parseTerm  = parseTerm1 
        <|> parseTerm2

parseTerm1 :: Parser Char Term
parseTerm1  = do
    f <- parseFactor
    o <- parseOpT
    t <- parseTerm
    many whitespace
    return $ Trm1 f o t

parseTerm2 :: Parser Char Term
parseTerm2  = do
    f <- parseFactor
    many whitespace
    return $ Trm2 f

parseFactor :: Parser Char Factor
parseFactor  = parseNumber 
           <|> parseFac2 
           <|> parseFac3

parseFac2 :: Parser Char Factor
parseFac2  = do
    o <- parseUnOp
    f <- parseFactor
    many whitespace
    return $ Fac2 o f

parseFac3 :: Parser Char Factor
parseFac3  = do
    e <- parens parseExp
    many whitespace
    return $ Fac3 e

parseNumber :: Parser Char Factor
parseNumber  = do
    n <- parseNumber'
    many whitespace
    return n
  where
    parseNumber' :: Parser Char Factor
    parseNumber'  = Parser (\s -> case getInt s of
                                Nothing -> Left "Can't parse int"
                                Just (i,s') -> Right (Number (read i), s'))

    getInt :: String -> Maybe (String, String)
    getInt [] = Nothing
    getInt (x:xs) | x `elem` "0123456789" 
                        = case getInt xs of
                                  Just (s1,s2) -> Just (x:s1, s2)
                                  Nothing      -> Just ([x] , xs)
    getInt _ = Nothing

parseOpE :: Parser Char OpE
parseOpE  = do
    o <- lexeme $ sat (`elem` "-+")
    many whitespace
    case o of
      '-' -> return Sub
      '+' -> return Add
      _   -> error "Parse error: failed while parsing + or -"
        
parseOpT :: Parser Char OpT
parseOpT  = do
    o <- lexeme $ sat (`elem` "/*")
    many whitespace
    case o of
      '/' -> return Div
      '*' -> return Mul
      _   -> error "Parse error: failed while parsing / or *"
        
parseUnOp :: Parser Char UnOp
parseUnOp  = do
    o <- lexeme $ sat (`elem` "-+")
    many whitespace
    case o of
      '+' -> return NotNeg
      '-' -> return Neg
      _   -> error "Parse error: failed while parsing + or -"


--------------------------------------------------------------------------------
--  Interpreter

eval  :: Exp -> Integer
eval e = evale e `mod` modAmount

modAmount :: Integer
modAmount  = 10^(9::Int) + 7

evale               :: Exp -> Integer
evale (Exp1 t Sub e) = evalt t - evale e
evale (Exp1 t Add e) = evalt t + evale e 
evale (Exp2 t)       = evalt t

evalt               :: Term -> Integer
evalt (Trm1 f Div t) = evalf f * powm (evalt t) (modAmount-2) modAmount 1
evalt (Trm1 f Mul t) = evalf f * evalt t
evalt (Trm2 f)       = evalf f

evalf                :: Factor -> Integer
evalf (Number i)      = i
evalf (Fac2 Neg f)    = - (evalf f)
evalf (Fac2 NotNeg f) = evalf f
evalf (Fac3 e)        = evale e

powm                  :: Integer -> Integer -> Integer -> Integer -> Integer
powm _ 0 _ r           = r
powm b e m r 
      | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r           = powm (b * b `mod` m) (e `div` 2) m r


--------------------------------------------------------------------------------
--  Main

main :: IO ()
main = do
    expression <- getLine
    let ex = parse parseExp expression
        ans = case ex of
            Right (e,_) -> show $ eval e
            Left err -> show err
    putStrLn ans

{% endhighlight %}
