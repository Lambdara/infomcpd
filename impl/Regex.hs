module Regex where

import Parser -- This is here just for debug purposes. If this is still here in the pull request then you should not merge xd
import AST
import Text.Parsec

-- This is here just for debug purposes. If this is still here in the pull request then you should not merge xd
testRegex :: Regex
Right testRegex = parse (pRegexTill ' ') "" "a* "

regex :: Regex -> String -> Maybe (String, String, [String])
regex reg str =
  case result of
    Just (pre,post,match,groups) -> Just (pre,post,(match:groups))
    Nothing -> Nothing
  where result = regtry reg' str "" (take n (repeat ""))
        (reg',n) = regpp reg 0

regtry :: Regex -> String -> String -> [String] -> Maybe (String, String, String, [String])
regtry reg str pre groups =
  case result of
    Just (post, match, groups') -> Just (pre, post, match, groups')
    Nothing -> result'
      where result' = regtry reg str' (pre ++ [c]) groups
            (c:str') = str
  where result = r reg str (pre == "") groups

regpp :: Regex -> Int -> (Regex, Int)
regpp (RegConcat []) n = ((RegConcat []), n)
regpp (RegConcat ((RegGroup r1 _):rs)) n = (RegConcat ((RegGroup r1' (n+1)):r'),m')
  where ((RegConcat r'),m') = regpp (RegConcat rs) m
        (r1',m) = regpp r1 (n+1)
regpp (RegConcat (x:rs)) n = ((RegConcat (x:r')),n')
  where ((RegConcat r'),n') = (regpp (RegConcat rs) n)
regpp _ _ = error "regpp expects RegConcat"

r :: Regex -> String -> Bool -> [String] -> Maybe (String, String, [String])
r (RegConcat []) s _ groups = Just (s,"",groups)
r (RegConcat ((RegChar c):rs)) (c':cs) _ groups =
  if c == c'
  then do
    (post,match,groups') <- r (RegConcat rs) cs False groups
    return (post, (c:match), groups')
  else Nothing
r (RegConcat ((RegStar r1):rs)) s begin groups =
  if result /= Nothing
  then result
  else result'
  where
    result = r (RegConcat (r1:(RegStar r1):rs)) s begin groups
    result' = r (RegConcat rs) s begin groups
r _ _ _ _ = error "unimplemented r"
