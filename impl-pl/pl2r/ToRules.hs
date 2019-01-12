module ToRules(toRules) where

import Data.Maybe
import Debug.Trace

import Prolog
import Rules

toRules :: Program -> Rules
toRules (Program stmts) = Rules (catMaybes $ map toRule stmts)

toRule :: Stmt -> Maybe Rule
toRule (Annot _) = Nothing
toRule (Fact term@(Pred name _) pes) =
    Just $ Rule name (map peToJudge pes) (termToJudge term)
toRule (Fact term pes) = toRule (Fact term pes)
toRule stmt = traceShow stmt undefined

peToJudge :: PredExpr -> Judge
peToJudge (Expr expr) = Judge $ formatExpr expr
peToJudge (Not (Term (Pred "member" [t1, t2]))) =
    Judge $ formatTerm t1 ++ [Math "\\not\\in"] ++ formatTerm t2
peToJudge (Not term) = traceShow (Not term) undefined

termToJudge :: Term -> Judge
termToJudge term = Judge $ formatTerm term

formatExpr :: Expr -> [FItem]
formatExpr (Binop bo e1 e2) = [Parens (formatExpr e1 ++ formatBO bo ++ formatExpr e2)]
formatExpr (Term term) = formatTerm term

formatTerm :: Term -> [FItem]
formatTerm (Pred "member" [t1, t2]) = formatTerm t1 ++ [Math "\\in"] ++ formatTerm t2
formatTerm (Pred name args) = Keyword name : concatMap formatTerm args
formatTerm (List []) = [Argument "\\epsilon"]
formatTerm (List [term]) = formatTerm term
formatTerm (List terms) = [Parens (concatMap formatTerm terms)]
formatTerm (Section t1 t2) = [Parens (formatTerm t1 ++ [Argument "|"] ++ formatTerm t2)]
formatTerm (LitNum n) = [Math (show n)]
formatTerm (LitStr s) = [Code ("\"" ++ s ++ "\"")]
formatTerm (Var name) = [Argument name]
formatTerm (TExpr expr) = formatExpr expr

formatBO :: Binop -> [FItem]
formatBO BPlus = [Math "+"]
formatBO BMinus = [Math "-"]
formatBO BEq = [Math "="]
formatBO BNeq = [Math "\\not="]
formatBO BGeq = [Math "\\geq"]
formatBO BLeq = [Math "\\leq"]
formatBO BGt = [Math ">"]
formatBO BLt = [Math "<"]
