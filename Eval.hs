module Eval where

import Prelude
import Debug.Trace
import Data.Char (isDigit, isSpace)

-- S -> N | '(' S O S ')'
-- G -> N | N.N
-- N -> 0..9 | 0..9 N
-- O -> + | - | * | / | % | ^

data State = S | G | N | O deriving (Show, Eq)
data Operator = Plus | Minus | Times | Divide | Modulus | Power deriving (Show, Eq)
data ExprTree = Leaf String | Node Operator ExprTree ExprTree deriving (Show, Eq)  

isOperator x = x `elem` "+-*/%^"
toOperator '+' = Plus
toOperator '-' = Minus
toOperator '*' = Times
toOperator '/' = Divide
toOperator '%' = Modulus
toOperator '^' = Power
toOperator x   = error $ "Expected operator, but found " ++ [x]

-- Parse a string; starting with state S
parse xs
    | r == ""   = t
    | otherwise = error $ "Unexpected remainder " ++ r
    where (t,r) = parser S xs

-- Parser, walks through all states
parser :: State -> String -> (ExprTree, String)
-- State S: a number or an expression in brackets
parser S (' ':xs) = parser S xs
parser S ('(':xs)
    | e = (Node o t1 t2, er)
    where (t1, r1) = parser S xs
          (o, or) = parseOperator (r1)
          (t2, r2) = parser S (or)
          (e, er) = parseEnd r2
          
parser S n@(x:xs) = parser N n

-- A number
parser N xs
    | p /= []   = (Leaf p, rp)
    | otherwise = error "Expected number, variable or expression in parenthesis, but none found"
    where (p, rp) = parseNumber xs

-- Anything else
parser _ (x:xs)   = error ("Unexpected character " ++ [x])
parser _ _        = error "Unexpected end of string"

-- Parse a number
parseNumber n@(':':xs) = parseVariable n
parseNumber n@('-':xs) = ('-':p, r)
    where
        (p,r) = parseNatNumber xs
parseNumber n@(x:xs)   = parseNatNumber n
parseNumber _          = error "Expected number or variable, but none found (2)"

-- Parse a variable
parseVariable (x:xs)
    | not (isOperator x || isDigit x || isSpace x || x == ')') = (x : p, ps)
    | otherwise                                                = ([], x:xs)
    where
        (p, ps) = parseVariable xs
parseVariable [] = ([],[])

-- Parse a natural number
parseNatNumber (x:xs)
    | isDigit x || x == '.' = (x : p, ps)
    | otherwise       = ([], x:xs)
    where
        (p, ps) = parseNatNumber xs
parseNatNumber [] = ([],[])

-- Parse an operator
parseOperator (' ':xs) = parseOperator xs
parseOperator (x:xs)   = (toOperator x, xs)
parseOperator _        = error "Expected operator, but none found"

-- Parse a line ending: true if found, or false if not
parseEnd (' ':xs) = parseEnd xs
parseEnd (')':xs) = (True, xs)
parseEnd x@(_:xs) = (False, x)
parseEnd []       = (False, "")

-- Evaluate an expression. evalA accepts an argument list.
eval :: String -> Float
eval = evalA []
evalA argmap = (evalR argmap) . parse 

evalR argmap (Leaf n)       = getVal argmap n
evalR argmap (Node o t1 t2) = calc o (evalR argmap t1) (evalR argmap t2)

-- Calculations
calc :: Operator -> Float -> Float -> Float
calc Plus x y = x+y
calc Minus x y = x-y
calc Times x y = x*y
calc Divide x y = x/y
calc Modulus x y = fromIntegral $ (floor x) `mod` (floor y)
calc Power x y = x^(floor y)

-- Get value
getVal argmap s
    | (all (`elem` "0123456789-.") s) = read s
    | may == Nothing                  = error $ "Argument " ++ s ++ " undefined"
    | otherwise                       = read param
    where
        may = lookup s argmap
        Just param = may