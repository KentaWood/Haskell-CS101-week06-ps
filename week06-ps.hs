import DFSM

import qualified Data.Set as Set
import Data.Char ()

-----------------------------PROBLEM 1-----------------------------
data OpCode = Push Float | Add | Mult | Sub | Div | Swap deriving Show
--the stack is shown as a list of floats
type Stack = [Float] 


eval :: ([OpCode], Stack) -> Float
eval ([], a:rest) = a  --pop the stack after no more operations                                
eval ((Push n):ops, rest) = eval (ops, n:rest)    -- push onto the stack 

-- eval: calculator operations using stack
-- eval ((OP[0]):rest_of_ops, a:b:rest_stack) = eval (rest_of_ops, (result of operation):rest_stack)
eval (Add:ops, a:b:rest) = eval (ops, (b + a):rest)   
eval (Mult:ops, a:b:rest) = eval (ops, (b * a):rest)  
eval (Sub:ops, a:b:rest) = eval (ops, (b - a):rest)   
eval (Div:ops, a:b:rest) = eval (ops, (b / a):rest)  
eval (Swap:ops, a:b:rest) = eval (ops, b:a:rest)   

--place holder ??   
eval (_, _) = 0.0                                     

-----------------------------PROBLEM 2-----------------------------



-----------------------------PROBLEM 3-----------------------------
data Tokens = AToken | LParen | RParen | Comma | Error String | EOF deriving (Eq,Show)

getTokens :: [Char] -> [Tokens]
getTokens [] = [EOF]
getTokens (x:xs) 
    | x == '(' =  LParen: getTokens xs
    | x == ')' = RParen: getTokens xs
    | x == 'a' = AToken: getTokens xs
    | x == ',' = Comma: getTokens xs
    | x == ' ' = getTokens xs
    | otherwise = [Error "Not Valid"] 

main :: IO ()
main = do
    print (eval([Push 2.0, Push 1.0, Sub],[]))
    print (getTokens "( (a,a),a,(a,a) )")
    print (getTokens "b,)")
