module AST where

-- An AST for a Prolog program.

type Name = String

data Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt = SFact Term RHS
          | SAnnot String
  deriving (Eq, Show)

data RHS = Conj [RHS]
         | Disj [RHS]
         | Not RHS
         | Is Name Expr
         | Expr Expr
  deriving (Eq, Show)

data Expr = Binop Binop Expr Expr
          | ETerm Term
  deriving (Eq, Show)

data Binop = BPlus | BMinus | BEq | BNeq | BGeq | BLeq | BGt | BLt
  deriving (Eq, Show)

data Term = Pred Name [Term]
          | List [Term]
          | Section Term Term  -- [H | T]
          | LitNum Int
          | LitStr String
          | Var Name
  deriving (Eq, Show)
