module Ast where

data Expr = Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Div Expr Expr
    | Literal Float

eval :: Expr -> Float
eval (Plus left right)  = eval left + eval right
eval (Minus left right) = eval left - eval right
eval (Times left right) = eval left * eval right
eval (Div numerator denominator)   = eval numerator / eval denominator
eval (Literal val)   = val

eq :: Expr -> Expr -> Bool
q (Literal x) (Literal y) = x == y
eq (Plus a b) (Plus c d)       = eq a c && eq b d
eq (Minus a b) (Minus c d)     = eq a c && eq b d
eq (Times a b) (Times c d)     = eq a c && eq b d
eq (Div a b) (Div c d)         = eq a c && eq b d
eq _ _ = False
-- Should eval to "5.0 "
test1 = Plus (Literal 3.0) (Literal 2.0)

-- Should eval to "3.5 "
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))

-- Should eval to "15.5"
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))

