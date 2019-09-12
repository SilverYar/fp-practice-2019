module Task1_1 where

import Todo(todo)


data BinaryOperation = Mult | Add | Sub
    deriving (Show, Eq)

-- тип данных
data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ op :: BinaryOperation, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant x) (IntConstant y) = IntConstant (x + y)
(|+|) x y = BinaryTerm Add x y

infixl 6 |+|
-- (IntConstant 5) |+| (IntConstant 6) == (IntConstant 11) == BinaryTerm Add (IntConstant 5) (IntConstant 6)
-- (IntConstant 6) |+| (Variable "x") == BinaryTerm Add (IntConstant 6) (Variable "x")
-- (IntConstant 6) |+| (Variable "x") |+| (IntConstant 6) == (IntConstant 12) |+| (Variable "x")

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant x) (IntConstant y) = IntConstant (x - y)
(|-|) x y = BinaryTerm Sub x y

infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant x) (IntConstant y) = IntConstant (x * y)
(|*|) x y = BinaryTerm Mult x y

infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`


replaceVar :: String -> Term -> Term -> Term
replaceVar _ _ i@(IntConstant x) = i
replaceVar varName replacement v@(Variable x) | (x == varName) = replacement
                                              | otherwise = v
replaceVar varName replacement b@(BinaryTerm o l r) = BinaryTerm o (replaceVar varName replacement l) (replaceVar varName replacement r)                                               



-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Add (IntConstant x) (IntConstant y)) = IntConstant (x + y)
evaluate (BinaryTerm Add (IntConstant 0) term) = term
evaluate (BinaryTerm Add term (IntConstant 0)) = term

evaluate (BinaryTerm Mult (IntConstant x) (IntConstant y)) = IntConstant (x * y)
evaluate (BinaryTerm Mult (IntConstant 0) term) = IntConstant 0
evaluate (BinaryTerm Mult term (IntConstant 0)) = IntConstant 0
evaluate (BinaryTerm Mult (IntConstant 1) term) = term
evaluate (BinaryTerm Mult term (IntConstant 1)) = term

evaluate (BinaryTerm Sub (IntConstant x) (IntConstant y)) = IntConstant (x - y)
evaluate (BinaryTerm Sub term (IntConstant 0)) = term

evaluate (BinaryTerm op x y) = evaluate (BinaryTerm op (evaluate x) (evaluate y))
evaluate exp = exp

--Test
-- evaluate (BinaryTerm Mult (BinaryTerm Add (IntConstant 5) (IntConstant 6)) (BinaryTerm Add (IntConstant 5) (IntConstant 6))) = IntConstant {intValue = 121}
-- replaceVar "x" (IntConstant 10) (BinaryTerm Add (Variable "x") (Variable "x"))