{-# OPTIONS_GHC -Wall #-}
module Polish where

import Data.List

type OperatorStack = [String]
type RPNList = [String]

precedence :: String -> Integer
precedence "ln" = 5
precedence "sin" = 5
precedence "cos" = 5
precedence "^" = 4
precedence "/" = 3
precedence "*" = 3
precedence "+" = 2
precedence "-" = 2
precedence _ = error "invalid operator in priority"

isLeftAssociative :: String -> Bool
isLeftAssociative "^" = False
isLeftAssociative _ = True

isNumeric :: String -> Bool
isNumeric s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

isVar :: String -> Bool
isVar s  = all (`elem` ['a'..'z']) s

isFunction :: String -> Bool
isFunction x = x `elem` ["ln", "sin", "cos"]

isOperator :: String -> Bool
isOperator x = x `elem` ["+", "-", "*", "/", "^"]

shuntingYard :: [String] -> RPNList -> OperatorStack -> [String]
shuntingYard [] rpnList opStack = (reverse rpnList) ++ opStack
shuntingYard (x:xs) rpnList opStack
    | isNumeric x = shuntingYard xs (x:rpnList) opStack
    | isFunction x = shuntingYard xs rpnList (x:opStack)
    | isOperator x = shuntingYard xs ((reverse (fst (break (compareOperators x) opStack))) ++ rpnList)
                                     (x:(snd (break (compareOperators x) opStack))) 

    | x == "("     = shuntingYard xs rpnList (x:opStack)
    | x == ")"     = shuntingYard xs ((reverse (fst (break ("("==) opStack))) ++ rpnList) (tail (snd (break ("("==) opStack)))
    | isVar x      = shuntingYard xs (x:rpnList) opStack
    | otherwise    = error "input error shuntingYard"

compareOperators :: String -> String -> Bool
compareOperators op str 
    | str == "(" = True
    | isFunction str = False
    | precedence str > precedence op = False
    | (precedence str == precedence op) && (isLeftAssociative str) = False
    | otherwise = True

convertToRPN :: String -> String
convertToRPN str = intercalate " " (shuntingYard (words str) [] [])


-- input - RPN string -----------------------------------
solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs
            foldingFunction (x:xs) "sin" = sin x:xs
            foldingFunction (x:xs) "cos" = cos x:xs   
            foldingFunction xs numberString = read numberString:xs 


-- Tests
-- (1)
-- convertToRPN "3 + 2 - 5"
-- answer: "3 2 + 5 -"
--
-- solveRPN "3 2 + 5 -"
-- answer: 0.0
--
-- (2)
-- convertToRPN "10 - ( 4 + 3 ) * 2"
-- answer: "10 4 3 + 2 * -"
--
-- solveRPN "10 4 3 + 2 * -"
-- answer: -4.0
--
-- (3)
-- convertToRPN "2 + 512 / 2 ^ 0.5 ^ 2"
-- answer: "2 512 2 0.5 2 ^ ^ / +"
--
-- solveRPN "2 512 2 0.5 2 ^ ^ / +"
-- answer: 432.53897
--
-- (4)
-- convertToRPN "( cos ( sin 0 ) + 2 ) ^ 2"
-- answer: "0 sin cos 2 + 2 ^"
--
-- solveRPN "0 sin cos 2 + 2 ^"
-- answer: 9.0
--
-- (5)
-- convertToRPN "a + b * c"
-- answer: "a b c * +"
--
-- (6)
-- convertToRPN "( a + b ) ^ c"
-- answer: "a b + c ^"
--
-- (7)
-- convertToRPN "cos a * ( b - c ) ^ d"
-- answer: "a cos b c - d ^ *"
