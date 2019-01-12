module Prolog where

-- An AST for a Prolog program.

type Name = String

data Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt = Fact Term [PredExpr]
          | Annot String
  deriving (Eq, Show)

data PredExpr = Is Name Expr
              | Expr Expr
              | Not Expr
  deriving (Eq, Show)

data Expr = Binop Binop Expr Expr
          | Term Term
  deriving (Eq, Show)

data Binop = BPlus | BMinus | BEq | BNeq | BGeq | BLeq | BGt | BLt
  deriving (Eq, Show)

data Term = Pred Name [Term]
          | List [Term]
          | Section Term Term  -- [H | T]
          | LitNum Int
          | LitStr String
          | Var Name
          | TExpr Expr
  deriving (Eq, Show)
