module ToRules(toRules) where

import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace
import Text.Parsec

import Prolog
import Rules

data Options = Options
    { oVars :: Map.Map Name Name
    , oAsSet :: Bool
    , oLeft :: Map.Map Name Int }
  deriving (Eq, Show)

baseOptions :: Options
baseOptions = Options Map.empty False Map.empty

optNewPred :: Options -> Options
optNewPred opts = opts { oVars = Map.empty, oAsSet = False }

toRules :: Program -> Rules
toRules (Program stmts) = 
    let (_, _, res) = foldl go ("", baseOptions, []) stmts
    in Rules (reverse res)
  where
    go :: (Name, Options, [Rule]) -> Stmt -> (Name, Options, [Rule])
    go (prev, opts, rules) (Annot s) = (prev, parseAnnot s opts, rules)
    go (prev, opts, rules) fact@(Fact (Pred name _) _) =
        (name,
         if name == prev then opts else optNewPred opts,
         toRule opts fact : rules)

parseAnnot :: String -> Options -> Options
parseAnnot str opts = case runParser pLine opts str str of
    Right newopts -> newopts
    Left err -> error (show err)
  where
    pLine = spaces >> sepBy pItem (spaces >> char ',' >> spaces) >> spaces >> eof >> getState
    pItem = pVar <|> pLeft
    pVar = string "var " >> pName >>= \a -> char '=' >> pName >>= \b -> addVar a b
    pLeft = string "left " >> pName >>= \name -> space >> spaces >> pInt >>= \n -> setLeft name n
    pName = many1 alphaNum
    pInt = read <$> many1 digit
    addVar from to = modifyState $ \o -> o { oVars = Map.insert from to (oVars o) }
    setLeft name n = modifyState $ \o -> o { oLeft = Map.insert name n (oLeft o) }

toRule :: Options -> Stmt -> Rule
toRule opts (Fact term@(Pred name _) pes) =
    Rule name (map (peToJudge opts) pes) (termToJudge opts term)
toRule opts (Fact term pes) = toRule opts (Fact term pes)
toRule _    (Annot _) = undefined

peToJudge :: Options -> PredExpr -> Judge
peToJudge opts (Expr expr) = Judge $ formatExpr opts expr
peToJudge opts (Not (Term (Pred "member" [t1, t2]))) =
    Judge $ formatTerm opts t1 ++ [Math "\\not\\in"] ++ formatTerm (opts { oAsSet = True }) t2
peToJudge _    (Not term) = traceShow (Not term) undefined

termToJudge :: Options -> Term -> Judge
termToJudge opts term = Judge $ formatTerm opts term

formatExpr :: Options -> Expr -> [FItem]
formatExpr opts (Binop bo e1 e2) =
    [Parens (formatExpr opts e1 ++ formatBO bo ++ formatExpr opts e2)]
formatExpr opts (Term term) = formatTerm opts term

formatTerm :: Options -> Term -> [FItem]
formatTerm _ (Pred name _) | name `elem` ignorePred = []
formatTerm opts (Pred name args) = formatPred opts name args True
formatTerm opts term = formatTerm' opts term

formatTerm' :: Options -> Term -> [FItem]
formatTerm' opts (Pred "member" [t1, t2]) = formatTerm' opts t1 ++ [Math "\\in"] ++ formatTerm' (opts { oAsSet = True }) t2
formatTerm' _ (Pred name _) | name `elem` ignorePred = []
formatTerm' opts (Pred name args) = formatPred opts name args False
formatTerm' opts (List []) = [Math (if oAsSet opts then "\\emptyset" else "\\epsilon")]
formatTerm' opts (List l)
    | oAsSet opts = [Math "\\{"] ++ intercalate [Math ","] (map (formatTerm' opts2) l) ++ [Math "\\}"]
    | otherwise = case l of
        [term] -> formatTerm' opts2 term
        _ -> [Parens (concatMap (formatTerm' opts2) l)]
  where
    opts2 = opts { oAsSet = False }
formatTerm' opts (Section t1 t2) = [Parens (formatTerm' opts t1 ++ formatTerm' opts t2)]
formatTerm' _    (LitNum n) = [Math (show n)]
formatTerm' _    (LitStr s) = [Code ("\"" ++ s ++ "\"")]
formatTerm' opts (Var name) = [Argument (case Map.lookup name (oVars opts) of { Just n -> n ; Nothing -> map toLower name })]
formatTerm' opts (TExpr expr) = formatExpr opts expr

formatPred :: Options -> Name -> [Term] -> Bool -> [FItem]
formatPred opts name args toplevel =
    let useparens = not toplevel && not (null args)
        (left, right) = splitAt (fromMaybe 0 $ Map.lookup name (oLeft opts)) $ map (formatTerm' opts) args
        res = concat left ++ [Keyword name] ++ concat right
    in if useparens then [Parens res] else res

ignorePred :: [Name]
ignorePred = ["write", "nl", "writeq"]

formatBO :: Binop -> [FItem]
formatBO BPlus = [Math "+"]
formatBO BMinus = [Math "-"]
formatBO BEq = [Math "="]
formatBO BNeq = [Math "\\not="]
formatBO BGeq = [Math "\\geq"]
formatBO BLeq = [Math "\\leq"]
formatBO BGt = [Math ">"]
formatBO BLt = [Math "<"]
