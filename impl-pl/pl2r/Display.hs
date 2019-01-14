module Display(display) where

import Data.Char
import Data.List

import Rules

display :: Rules -> String
display (Rules rs) = intercalate "\n\n" (map displayR rs)

displayR :: Rule -> String
displayR (Rule name reqs concl) =
    let line1 = "\\[ "
                    ++ "\\RULE"
        indent = replicate (length line1) ' '
        line2 = "[" ++ safeText name ++ "]"
        line3 = "{"
                    ++ intercalate (" \\qquad\n" ++ indent ++ " ")
                                   (map displayJ (filter (not . isEmpty) reqs))
                    ++ "}"
        line4 = "{" ++ displayJ concl ++ "}"
                    ++ " \\]"
    in line1 ++ intercalate ("\n" ++ indent) [line2, line3, line4]
  where
    isEmpty (Judge l) = null l

displayJ :: Judge -> String
displayJ (Judge items) = intercalate "\\ " (map displayF items)

displayF :: FItem -> String
displayF (Keyword s) = "\\J{" ++ safeText s ++ "}"
displayF (Argument s) = "\\arg{" ++ safeMath s ++ "}"
displayF (Command s) = "\\cmd{" ++ safeText s ++ "}"
displayF (Math s) = safeMath s
displayF (Code s) = "\\code{" ++ safeText s ++ "}"
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
safeTextChar '_' = "\\_{}"
safeTextChar c = [c]
