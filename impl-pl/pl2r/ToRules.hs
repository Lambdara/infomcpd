{-# LANGUAGE MultiWayIf #-}
module ToRules(toRules) where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec

import Prolog
import Rules

data Options = Options
    { oVars :: Map.Map Name Name
    , oMathVars :: Set.Set Name
    , oArg :: Map.Map (Name, Int) ArgStyle
    , oAsSet :: Bool
    , oAsStack :: Bool
    , oLeft :: Map.Map Name Int
    , oSkip :: Set.Set Name
    , oData :: Map.Map Name DataStyle
    , oPredName :: Map.Map Name Name }
  deriving (Eq, Show)

data DataStyle = DPred | DTuple | DCmd
  deriving (Eq, Show)

data ArgStyle = ANormal | AStack
  deriving (Eq, Show)

baseOptions :: Options
baseOptions = Options Map.empty Set.empty Map.empty False False Map.empty Set.empty Map.empty Map.empty

optNewPred :: Options -> Options
optNewPred opts = opts { oVars = Map.empty, oMathVars = Set.empty, oAsSet = False, oAsStack = False }

toRules :: Program -> Rules
toRules (Program stmts) = 
    let (_, res) = go (baseOptions, []) stmts
    in Rules (reverse res)
  where
    go :: (Options, [Rule]) -> [Stmt] -> (Options, [Rule])
    go (opts, rules) [] = (opts, rules)
    go (opts, rules) (Annot s : rest) = go (parseAnnot s opts, rules) rest
    go (opts, rules) (fact@(Fact (Pred name _) _) : rest)
        | name `Set.member` oSkip opts = go (opts, rules) rest
        | otherwise =
            let newopts = if name /= findPredName rest then optNewPred opts else opts
            in go (newopts, toRule opts fact : rules) rest
    go _ _ = undefined

    findPredName :: [Stmt] -> Name
    findPredName [] = ""
    findPredName (Fact (Pred name _) _ : _) = name
    findPredName (_ : rest) = findPredName rest

parseAnnot :: String -> Options -> Options
parseAnnot str opts = case runParser pLine opts str str of
    Right newopts -> newopts
    Left err -> error (show err)
  where
    pLine = sepBy (spaces >> pItem) (spaces >> char ',') >> spaces >> eof >> getState
    pItem = pVar <|> pMathVar <|> pArg <|> pLeft <|> pSkip <|> pData <|> pPredName

    pVar = string "var " >> pName >>= \a -> char '=' >> pName >>= \b -> addVar a b
    pMathVar = string "mathvar " >> pName >>= \name -> addMathVar name
    pArg = string "arg " >> pName >>= \name -> space >> spaces >> pInt >>= \num -> space >> spaces >> pName >>= \style -> addArg name num style
    pLeft = string "left " >> pName >>= \name -> space >> spaces >> pInt >>= \n -> setLeft name n
    pSkip = string "skip " >> pName >>= \name -> addSkip name
    pData = string "data " >> pName >>= \name -> space >> spaces >> pName >>= \style -> addData name style
    pPredName = string "predname " >> pName >>= \from -> space >> spaces >> pName >>= \to -> addPredName from to

    pName = many1 (alphaNum <|> oneOf "_':\\")
    pInt = read <$> many1 digit

    addVar from to = modifyState $ \o -> o { oVars = Map.insert from to (oVars o) }
    addMathVar name = modifyState $ \o -> o { oMathVars = Set.insert name (oMathVars o) }
    addArg name num style = modifyState $ \o -> o { oArg = Map.insert (name, num) (parseArg style) (oArg o) }
    setLeft name n = modifyState $ \o -> o { oLeft = Map.insert name n (oLeft o) }
    addSkip name = modifyState $ \o -> o { oSkip = Set.insert name (oSkip o) }
    addData name style = modifyState $ \o -> o { oData = Map.insert name (parseData style) (oData o) }
    addPredName from to = modifyState $ \o -> o { oPredName = Map.insert from to (oPredName o) }

    parseData "pred" = DPred
    parseData "tuple" = DTuple
    parseData "cmd" = DCmd
    parseData style = error $ "Unknown data style '" ++ style ++ "'"

    parseArg "normal" = ANormal
    parseArg "stack" = AStack
    parseArg style = error $ "Unknown arg style '" ++ style ++ "'"

toRule :: Options -> Stmt -> Rule
toRule opts (Fact term@(Pred name _) pes) =
    let predname = fromMaybe name $ Map.lookup name (oPredName opts)
    in Rule predname (map (peToJudge opts) pes) (termToJudge opts term)
toRule opts (Fact term pes) = toRule opts (Fact term pes)
toRule _    (Annot _) = undefined

peToJudge :: Options -> PredExpr -> Judge
peToJudge opts (Expr expr) = Judge $ formatExpr opts expr
peToJudge opts (Not (Term (Pred "member" [t1, t2]))) =
    Judge $ joinMath [formatTerm opts t1, [Math "\\not\\in"], formatTerm (opts { oAsSet = True }) t2]
peToJudge _    (Not term) = traceShow (Not term) undefined
peToJudge _    _ = undefined

termToJudge :: Options -> Term -> Judge
termToJudge opts term = Judge $ formatTerm opts term

formatExpr :: Options -> Expr -> [FItem]
formatExpr opts (Binop bo e1 e2) =
    [Parens (joinMath [formatExpr opts e1, formatBO bo, formatExpr opts e2])]
formatExpr opts (Term term) = formatTerm opts term

formatTerm :: Options -> Term -> [FItem]
formatTerm _ (Pred name _) | name `elem` ignorePred = []
formatTerm opts (Pred name args) = formatPred opts name args True
formatTerm opts term = formatTerm' opts term

formatTerm' :: Options -> Term -> [FItem]
formatTerm' _ (Pred name _) | name `elem` ignorePred = []
formatTerm' opts (Pred name args) = formatPred opts name args False
formatTerm' opts (List []) = [Math (if oAsSet opts then "\\emptyset" else "\\epsilon")]
formatTerm' opts (List l)
    | oAsSet opts = [Math "\\{"] ++ intercalate [Math ","] (map (formatTerm' opts2) l) ++ [Math "\\}"]
    | oAsStack opts = [Parens (joinMath $ intersperse [Math "\\blacktriangleright"] (map (formatTerm' opts2) l) ++ [[Math "\\blacktriangleright\\bullet"]])]
    | otherwise = case l of
        [term] -> formatTerm' opts2 term
        _ -> [Parens (concatMap (formatTerm' opts2) l)]
  where
    opts2 = opts { oAsSet = False, oAsStack = False }
formatTerm' opts (Section t1 t2) =
    let (f1, f2) = (formatTerm' (opts { oAsStack = False }) t1, formatTerm' opts t2)
    in if oAsStack opts
           then [Parens (joinMath [f1, [Math "\\blacktriangleright"], f2])]
           else [Parens (f1 ++ f2)]
formatTerm' opts (Append t1 t2) =
    let terms = collectAppends (Append t1 t2)
    in [Parens (concatMap (formatTerm' opts) terms)]
formatTerm' _    (LitNum n) = [Math (show n)]
formatTerm' _    (LitStr s) = [Code ("\"" ++ s ++ "\"")]
formatTerm' opts (Var name) =
    let constr = if name `Set.member` oMathVars opts then Math else Argument
    in [constr (case Map.lookup name (oVars opts) of { Just n -> n ; Nothing -> map toLower name })]
formatTerm' opts (TExpr expr) = formatExpr opts expr

formatPred :: Options -> Name -> [Term] -> Bool -> [FItem]
formatPred opts "member" [t1, t2] True =
    joinMath [formatTerm' opts t1, [Math "\\in"], formatTerm' (opts { oAsSet = True }) t2]
formatPred opts name args toplevel =
    let style = if toplevel then DPred else fromMaybe DPred $ Map.lookup name (oData opts)
    in case style of
        DTuple ->
            [Parens (joinMath $ intersperse [Math ","] $ formatArgs opts)]
        _ ->
            let useparens = not toplevel && not (null args)
                numleft = if not toplevel then 0 else fromMaybe 0 $ Map.lookup name (oLeft opts)
                (left, right) = splitAt numleft $ formatArgs opts
                predname = fromMaybe name $ Map.lookup name (oPredName opts)
                keyword = if | style == DCmd -> Command predname
                             | name == "_" -> Argument "_"
                             | otherwise -> Keyword predname
                res = concat left ++ [keyword] ++ concat right
            in if useparens then [Parens res] else res
  where
    formatArgs opts' =
        zipWith (\arg idx ->
                    let opts'' = case Map.lookup (name, idx) (oArg opts') of
                                     Just AStack -> opts' { oAsStack = True }
                                     _ -> opts'
                    in formatTerm' opts'' arg)
                args [1..]

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

collectAppends :: Term -> [Term]
collectAppends (Append t1 t2) = collectAppends t1 ++ collectAppends t2
collectAppends (TExpr (Term t)) = collectAppends t
collectAppends t = [t]

joinMath :: [[FItem]] -> [FItem]
joinMath terms = foldl1 joinMath' (map arg2math terms)
  where
    arg2math [Argument str] | simpleStr str = [Math str]
    arg2math l = l

    simpleStr "\\not\\in" = True
    simpleStr "\\in" = True
    simpleStr [c] | c `elem` "+-" = True
    simpleStr s | all simpleChar s = True
    simpleStr _ = False

    simpleChar c | isAlphaNum c = True
                 | c `elem` "_'\\" = True
    simpleChar _ = False

joinMath' :: [FItem] -> [FItem] -> [FItem]
joinMath' [Math a] [Math b] = [Math (a ++ " " ++ b)]
joinMath' a b = a ++ b
