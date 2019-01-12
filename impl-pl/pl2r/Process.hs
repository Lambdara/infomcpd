module Process(process) where

import qualified Data.Map.Strict as Map

import Prolog

process :: Program -> Program
process (Program stmts) = Program (map processS stmts)

processS :: Stmt -> Stmt
processS (Fact term rhs) =
    let (mp, rhs') = collectIs rhs
    in Fact (processT (replaceVarsT mp term)) (map (processP . replaceVarsP mp) rhs')
processS (Annot str) = Annot str

processP :: PredExpr -> PredExpr
processP pe = pe

processT :: Term -> Term
processT term = term

replaceVarsP :: Map.Map Name Expr -> PredExpr -> PredExpr
replaceVarsP mp (Is name expr) = Is name (replaceVarsE mp expr)
replaceVarsP mp (Expr expr) = Expr (replaceVarsE mp expr)
replaceVarsP mp (Not expr) = Not (replaceVarsE mp expr)

replaceVarsE :: Map.Map Name Expr -> Expr -> Expr
replaceVarsE mp (Binop bo e1 e2) = Binop bo (replaceVarsE mp e1) (replaceVarsE mp e2)
replaceVarsE mp (Term term) = Term (replaceVarsT mp term)

replaceVarsT :: Map.Map Name Expr -> Term -> Term
replaceVarsT mp (Pred name terms) = Pred name (map (replaceVarsT mp) terms)
replaceVarsT mp (List terms) = List (map (replaceVarsT mp) terms)
replaceVarsT mp (Section t1 t2) = Section (replaceVarsT mp t1) (replaceVarsT mp t2)
replaceVarsT _ (LitNum n) = LitNum n
replaceVarsT _ (LitStr n) = LitStr n
replaceVarsT mp (Var name) = case Map.lookup name mp of
    Just expr -> TExpr expr
    Nothing -> Var name
replaceVarsT mp (TExpr expr) = TExpr $ replaceVarsE mp expr

collectIs :: [PredExpr] -> (Map.Map Name Expr, [PredExpr])
collectIs pes = go Map.empty pes
  where
    go mp [] = (mp, [])
    go mp (Is name expr : rest) =
        case Map.lookup name mp of
            Just _ -> error "Duplicate 'is' terms not supported"
            Nothing -> go (Map.insert name expr mp) rest
    go mp (Expr expr : rest) = fmap (Expr expr :) (go mp rest)
    go mp (Not expr : rest) = fmap (Not expr :) (go mp rest)
