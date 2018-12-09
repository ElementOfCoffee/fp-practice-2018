
module Task1_1 where

import Todo(todo)

data Term = IntConstant { intValue::Int }
          | Variable { varName::String }
          | BinaryTerm { lhv::Term, op::Operator, rhv::Term} 
          deriving (Show, Eq)

data Operator = Plus | Minus | Mult deriving (Show, Eq)

infixl 6 <+>
(IntConstant a) <+> (IntConstant b) = IntConstant (a + b)
a <+> b = BinaryTerm a Plus b 

infixl 6 <->
(IntConstant a) <-> (IntConstant b) = IntConstant (a - b)
a <-> b = BinaryTerm a Minus b 

infixl 7 <*>
(IntConstant a) <*> (IntConstant b) = IntConstant (a * b)
a <*> b = BinaryTerm a Mult b 

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant c) _ _ = IntConstant c
replaceVar (Variable v) name term = if v == name then term else Variable v
replaceVar (BinaryTerm l op r) name term = BinaryTerm (replaceVar l name term) op (replaceVar r name term)

evaluate :: Term -> Term
evaluate bTerm@(BinaryTerm l op r) = evaluate' (evaluate l) op (evaluate r)
   where
    evaluate' :: Term -> Operator -> Term -> Term
    evaluate' (IntConstant c1) Plus  (IntConstant c2) = IntConstant $ c1 + c2
    evaluate' (IntConstant 0)  Plus  t                = t
    evaluate' t                Plus  (IntConstant 0)  = t
    evaluate' (IntConstant c1) Minus (IntConstant c2) = IntConstant $ c1 - c2
    evaluate' t                Minus (IntConstant 0)  = t
    evaluate' (IntConstant c1) Mult  (IntConstant c2) = IntConstant $ c1 * c2
    evaluate' (IntConstant 0)  Mult  _                = IntConstant 0
    evaluate' (IntConstant 1)  Mult  t                = t
    evaluate' _                Mult  (IntConstant 0)  = IntConstant 0
    evaluate' t                Mult  (IntConstant 1)  = t
    evaluate' l                op    r                = bTerm

evaluate term = term
