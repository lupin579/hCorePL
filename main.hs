module Main where
import qualified Data.Map as Map
import Ast
import Control.Monad (liftM2)

data Value = VInt Int
           | VBool Bool
           | VClosure Var Expr Env
           | VDummy String
           | VPair Value Value
           deriving (Show)

vEqual :: Value -> Value -> Maybe Bool
vEqual (VInt v1)     (VInt v2)     = return $ v1 == v2
vEqual (VBool v1)    (VBool v2)    = return $ v1 == v2
vEqual (VPair v1 v2) (VPair v3 v4) = do{ b1 <- vEqual v1 v3
                                       ; b2 <- vEqual v2 v4
                                       ; return $ b1 && b2
                                       }
vEqual _ _ = Nothing

type Env = Map.Map Var Value
 
eval :: Ast.Expr -> Env -> Maybe Value
eval (Ast.Integer i)     env = return (VInt i)
eval (Ast.Boolean b)     env = return (VBool b)
eval (Ast.Ident var)     env = Map.lookup var env
eval (Ast.Binop Ast.Add e1 e2) env = do{ VInt v1 <- eval e1 env
                                       ; VInt v2 <- eval e2 env
                                       ; return $ VInt $ v1 + v2
                                       }
eval (Ast.Binop Ast.Minus e1 e2) env = do{ VInt v1 <- eval e1 env
                                         ; VInt v2 <- eval e2 env
                                         ; return $ VInt $ v1 - v2
                                         }
eval (Ast.Binop Ast.Mult e1 e2) env = do{ VInt v1 <- eval e1 env
                                        ; VInt v2 <- eval e2 env
                                        ; return $ VInt $ v1 * v2
                                        }
eval (Ast.Binop Ast.Divide e1 e2) env = do{ VInt v1 <- eval e1 env
                                          ; VInt v2 <- eval e2 env
                                          ; return $ VInt $ div v1 v2
                                          }
eval (Ast.Binop Ast.Leq e1 e2) env = do{ VInt v1 <- eval e1 env
                                       ; VInt v2 <- eval e2 env
                                       ; return $ VBool $ v1 <= v2
                                       }
eval (Ast.Binop Ast.Equal e1 e2) env = do{ v1 <- eval e1 env
                                         ; v2 <- eval e2 env
                                         ; v  <- vEqual v1 v2
                                         ; return $ VBool v
                                         }
eval (Ast.Let var e1 e2) env = do{ v1 <- eval e1 env
                                 ; eval e2 (Map.insert var v1 env)
                                 }
eval (Ast.Rec var e1 e2) env = do{ vc <- eval e1 (Map.insert var (VDummy "function exclusively") env)
                                 ; let env' = Map.insert var vc env
                                   in eval e2 env'
                                 } 
eval (Ast.If eb e1 e2) env = do{ VBool b <- eval eb env
                               ; if b then eval e1 env else eval e2 env
                               }
eval (Ast.Pair e1 e2) env = do { v1 <- eval e1 env
                               ; v2 <- eval e2 env
                               ; return (VPair v1 v2)}
eval (Ast.Fun var e) env = do { return (VClosure var e env)}
eval (Ast.App e1 e2) env = let v = eval e1 env in
                           case v of 
                              Just c@(VClosure var e old_env) -> do{ val <- eval e2 env
                                                                   ; let old_env' = Map.map (subst c) old_env 
                                                                     in  eval e (Map.insert var val old_env')}
                              Just (VDummy s)                 -> Nothing

subst :: Value -> Value -> Value
subst c (VDummy _) = c
subst _ v          = v

env :: Env
env = Map.empty 

main :: IO ()
main = case eval Ast.expr4 env of
        Just v -> print v
        _      -> print "err"
