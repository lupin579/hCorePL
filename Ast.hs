module Ast
( Var
, Bop(..)
, Expr(..)
, expr0
, expr1
, expr2
, expr3
, expr4)where

expr0 :: Expr
expr0 = Rec "f" 
           (Fun "x" 
                (If (Binop Leq 
                           (Ident "x") 
                           (Integer 1)) 
                    (Integer 1)
                    (Binop Mult
                           (Ident "x")
                           (App (Ident "f") 
                                (Binop Minus 
                                       (Ident "x") 
                                       (Integer 1)))))) 
           (App (Ident "f")
                (Integer 10))

expr1 :: Expr
expr1 = Let "nr"
            (Fun "x"
                 (Binop Mult
                        (Integer 2)
                        (Ident "x")))
            (Rec "r"
                 (Fun "x"
                      (If (Binop Leq 
                                 (Ident "x") 
                                 (Integer 1)) 
                          (Integer 1)
                          (Binop Mult
                                 (App (Ident "nr")
                                      (Ident "x"))
                                 (App (Ident "r") 
                                      (Binop Minus 
                                             (Ident "x") 
                                             (Integer 1))))))
                 (App (Ident "r")
                      (Integer 5)))

expr2 :: Expr
expr2 = Let "nr"
            (Fun "x"
                 (Binop Mult
                        (Integer 2)
                        (Ident "x")))
            (Let "nr1"
                 (Fun "x"
                      (Binop Mult
                             (Integer 3)
                             (Ident "x")))
                 (App (Ident "nr")
                      (App (Ident "nr1")
                           (Integer 10))))
-- letrec r = (fun x -> if x <= 1 then 1 else x * (r x)) in
-- let nr = (fun x -> 2 * (r x)) in nr 5
expr3 :: Expr
expr3 = Rec "r"
            (Fun "x"
                 (If (Binop Leq 
                           (Ident "x") 
                           (Integer 1)) 
                     (Integer 1)
                     (Binop Mult
                            (Ident "x")
                            (App (Ident "r") 
                                 (Binop Minus 
                                        (Ident "x") 
                                        (Integer 1))))))
            (Let "nr"
                 (Fun "x"
                      (Binop Mult
                             (Integer 2)
                             (App (Ident "r")
                                  (Ident "x"))))
                 (App (Ident "nr")
                      (Integer 5)))

expr4 :: Expr
expr4 = Rec "f"
            (Fun "x"
                 (Let "id"
                      (Fun "x'"
                           (Binop Mult
                                  (Ident "x")
                                  (App (Ident "f")
                                       (Ident "x'"))))
                      (If (Binop Equal
                                 (Ident "x")
                                 (Integer 1))
                          (Integer 1)
                          (Binop Mult
                                 (App (Ident "id")
                                      (Binop Minus
                                             (Ident "x")
                                             (Integer 1)))
                                 (App (Ident "f")
                                      (Binop Minus
                                             (Ident "x")
                                             (Integer 1)))))))
            (App (Ident "f")
                 (Integer 3))

type Var = String

data Bop = Add
         | Minus
         | Mult
         | Divide
         | Leq
         | Equal
         deriving (Show)

data Expr = Integer Int
          | Boolean Bool
          | Ident Var
          | Binop Bop Expr Expr
          | Let Var Expr Expr
          | Rec Var Expr Expr
          | If Expr Expr Expr
          | Pair Expr Expr
          | Fun Var Expr
          | App Expr Expr
          deriving (Show)
