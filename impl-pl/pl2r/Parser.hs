{-# LANGUAGE LambdaCase #-}
module Parser(parseProgram) where

import Control.Monad
import Data.Char
import Text.Parsec
import Text.Parsec.Expr

import Prolog


type Parser = Parsec String ()

parseProgram :: String -> String -> Either ParseError Program
parseProgram fname source = parse pProgram fname source

pProgram :: Parser Program
pProgram = do
    prog <- Program <$> many pStmt
    pWhiteComment
    eof
    return prog

pStmt :: Parser Stmt
pStmt = pAnnot <|> pFact

pAnnot :: Parser Stmt
pAnnot = do
    try $ pWhitespace >> void (string "%%")
    pWhitespace
    Annot <$> manyTill anyChar (void (char '\n') <|> eof)

pFact :: Parser Stmt
pFact = do
    term <- pTerm
    expr <- (symbol ":-" >> sepBy pPredExpr (symbol ",")) <|> return []
    symbol "."
    return $ Fact term expr

pTerm :: Parser Term
pTerm = pVar <|> pPred <|> pList <|> pLitNum <|> pLitStr

pPred :: Parser Term
pPred = do
    name <- pName
    arglist <- option [] pArgList
    return $ Pred name arglist
  where
    pArgList = do
        void $ char '('  -- must come directly after predicate name, for some reason
        args <- sepBy pTerm (symbol ",")
        symbol ")"
        return args

pList :: Parser Term
pList = do
    symbol "["
    (symbol "]" >> return (List [])) <|> do
        first <- pTerm
        choice [symbol "," >> do
                    rest <- sepBy1 pTerm (symbol ",")
                    symbol "]"
                    return (List (first : rest)),
                symbol "|" >> do
                    rest <- pTerm
                    symbol "]"
                    return (Section first rest),
                symbol "]" >> return (List [first])]

pLitNum :: Parser Term
pLitNum = try $ pWhiteComment >> (LitNum . read <$> many1 digit)

pLitStr :: Parser Term
pLitStr = do
    try $ pWhiteComment >> void (char '"')
    cs <- manyTill pStrChar (char '"')
    return (LitStr cs)
  where
    pStrChar = satisfy (/= '\\') <|> try pEscape <|> try pOctalEscape
    pEscape = do
        void $ char '\\'
        (char 'n' >> return '\n') <|>
            (char 't' >> return '\t') <|>
            (char 'r' >> return '\r')
    pOctalEscape = do
        void $ char '\\'
        a <- oneOf ['0'..'7']
        b <- oneOf ['0'..'7']
        c <- oneOf ['0'..'7']
        void $ char '\\'
        return $ chr $ 64 * read [a] + 8 * read [b] + read [c]

pVar :: Parser Term
pVar = try $ do
    pWhiteComment
    optional (char '_')
    name <- (:) <$> initVarChar <*> many furtherVarChar
    return $ Var name
  where
    initVarChar = upper
    furtherVarChar = letter <|> char '_' <|> digit

pPredExpr :: Parser PredExpr
pPredExpr = pIs <|> pNot <|> (Expr <$> pExpr)
  where
    pIs = do
        Var name <- try (pVar <* symbol "is")
        expr <- pExpr
        return $ Is name expr
    pNot = symbol "\\+" >> (Not <$> pExpr)

pExpr :: Parser Expr
pExpr = buildExpressionParser opTable (Term <$> pTerm)
  where
    opTable = [[Infix (pBinop "+" BPlus) AssocLeft,
                Infix (pBinop "-" BMinus) AssocLeft],
               [Infix (pBinop "=<" BLeq) AssocNone,
                Infix (pBinop ">=" BGeq) AssocNone,
                Infix (pBinop ">" BGt) AssocNone,
                Infix (pBinop "<" BLt) AssocNone,
                Infix (pBinop "=" BEq) AssocNone,
                Infix (pBinop "\\=" BNeq) AssocNone]]

    pBinop s bo = symbol s >> return (Binop bo)

pName :: Parser Name
pName = try $ pWhiteComment >> ((:) <$> initNameChar <*> many furtherNameChar)
  where
    initNameChar = lower <|> char '_'
    furtherNameChar = letter <|> char '_' <|> digit

symbol :: String -> Parser ()
symbol s = do
    try $ pWhiteComment >> void (string s)
    eof <|> lookAhead (void $ satisfy (notInCategories s))
  where
    notInCategories :: String -> Char -> Bool
    notInCategories str subj =
        let forbidden = (if any (`elem` alphabetics) str then alphabetics else "")
                        -- ++ (if any (`elem` symbols) str then symbols else "")
                        ++ (if any (`elem` numbers) str then numbers else "")
            alphabetics = let a = ['a'..'z'] in a ++ map toUpper a
            -- symbols = "+-*/><=().,;:\\|[]"
            numbers = ['0'..'9']
        in not (subj `elem` forbidden)

pWhitespace :: Parser ()
pWhitespace = void $ many space

pPlainComment :: Parser ()
pPlainComment = do
    void $ string "%"
    eof <|> void (satisfy (/= '%'))
    void $ manyTill anyChar (void (char '\n') <|> eof)

pWhiteComment :: Parser ()
pWhiteComment = void $ sepBy pWhitespace pPlainComment
