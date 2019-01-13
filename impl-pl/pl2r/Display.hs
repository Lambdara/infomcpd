module Display(display) where

import Data.Char
import Data.List

import Rules

display :: Rules -> String
display (Rules rs) = intercalate "\n\n" (map displayR rs)

displayR :: Rule -> String
displayR (Rule name reqs concl) =
    -- "\\[ \\RULE[" ++ safeText name ++ "]"
    "\\[ \\RULE"
        ++ "{" ++ intercalate " \\qquad " (map displayJ (filter (not . isEmpty) reqs)) ++ "}"
        ++ "{" ++ displayJ concl ++ "} \\]"
  where
    isEmpty (Judge l) = null l

displayJ :: Judge -> String
displayJ (Judge items) = intercalate "\\ " (map displayF items)

displayF :: FItem -> String
displayF (Keyword s) = "\\text{" ++ safeText s ++ "}"
displayF (Argument s) = "\\mathit{" ++ safeMath s ++ "}"
displayF (Command s) = "\\text{\\texttt{" ++ safeText s ++ "}}"
displayF (Math s) = safeMath s
displayF (Code s) = "\\text{\\texttt{" ++ safeText s ++ "}}"
displayF (Parens items) = "(" ++ intercalate "\\ " (map displayF items) ++ ")"

safeText :: String -> String
safeText = concatMap safeTextChar

safeMath :: String -> String
safeMath "" = ""
safeMath ('\n':cs) = "\\hookleftarrow" ++ safeMath cs
safeMath "_" = "\\_"
safeMath ('_':c:cs) | not (isDigit c) && c /= '{' = "\\_" ++ safeMath (c:cs)
safeMath (c:cs) = c : safeMath cs

safeTextChar :: Char -> String
safeTextChar '\n' = "$\\hookleftarrow$"
safeTextChar '_' = "\\textunderscore{}"
safeTextChar c = [c]
