module Regex where

import AST
import Text.Parsec

type CapGroups = [Maybe String]

regex :: Regex -> String -> Maybe (String, String, CapGroups)
regex reg str =
  case result of
    Just (pre,post,match,groups) -> Just (pre, post, Just match : groups)
    Nothing -> Nothing
  where result = regtry reg' str "" (replicate n Nothing)
        (reg',n) = regpp reg 0

regtry :: Regex -> String -> String -> CapGroups -> Maybe (String, String, String, CapGroups)
regtry reg str pre groups =
  case result of
    Just (post, match, groups') -> Just (pre, post, match, groups')
    Nothing -> case str of
      (c:str') -> regtry reg str' (pre ++ [c]) groups
      "" -> Nothing
  where result = r reg str (pre == "") groups

regpp :: Regex -> Int -> (Regex, Int)
regpp (RegConcat []) n = (RegConcat [], n)
regpp (RegConcat (RegGroup r1 _ : rs)) n = (RegConcat (RegGroup r1' (n+1) : rs'),m')
  where (r1',m) = regpp r1 (n+1)
        (RegConcat rs', m') = regpp (RegConcat rs) m
regpp (RegConcat (RegStar r1 : rs)) n = (RegConcat (RegStar r1' : rs'), n'')
  where (r1',n') = regpp r1 n
        (RegConcat rs', n'') = regpp (RegConcat rs) n'
regpp (RegConcat (RegRep r1 k1 k2 : rs)) n = (RegConcat (RegRep r1' k1 k2 : rs'), n'')
  where (r1',n') = regpp r1 n
        (RegConcat rs', n'') = regpp (RegConcat rs) n'
regpp (RegConcat (RegConcat rs1 : rs)) n = regpp (RegConcat (rs1 ++ rs)) n
regpp (RegConcat (x:rs)) n = (RegConcat (x:rs'), n')
  where (RegConcat rs', n') = regpp (RegConcat rs) n
regpp ritem n = (ritem', n')
  where (RegConcat [ritem'], n') = regpp (RegConcat [ritem]) n

r :: Regex -> String -> Bool -> CapGroups -> Maybe (String, String, CapGroups)
r (RegConcat []) s _ groups = Just (s,"",groups)
r (RegConcat (RegChar c : rs)) (c':cs) _ groups =
  if c == c'
  then do
    (post, match, groups') <- r (RegConcat rs) cs False groups
    return (post, c:match, groups')
  else Nothing
r (RegConcat (RegClass polarity rcis : rs)) (c:cs) _ groups =
  if rcicheck rcis c == polarity
  then do
    (post, match, groups') <- r (RegConcat rs) cs False groups
    return (post, c:match, groups')
  else
    Nothing
  where
    -- This checks whether the character matches any of the RegClassItems
    rcicheck [] _ = False
    rcicheck (RCChar c1 : rcis') ch =
      c1 == ch || rcicheck rcis' ch
    rcicheck (RCRange c1 c2 : rcis') ch =
      ch `elem` [c1 .. c2] || rcicheck rcis' ch
r (RegConcat (RegBackref n : rs)) s begin groups
  | Just grp <- groups !! (n - 1) =
      let (s1, s2) = splitAt (length grp) s
      in if s1 == grp
         then do
           (post,match,groups') <- r (RegConcat rs) s2 (begin && null s1) groups
           return (post, s1 ++ match, groups')
         else Nothing
  | otherwise =
      Nothing
r (RegConcat (RegAnchorLeft : rs)) s begin groups =
  if begin
  then r (RegConcat rs) s begin groups
  else Nothing
r (RegConcat (RegAnchorRight : rs)) s begin groups =
  if s == ""
  then r (RegConcat rs) s begin groups
  else Nothing
r (RegConcat (RegGroup r1 n : rs)) s begin groups =
  do
    (post1, match1, groups1) <- r r1 s begin groups
    (post, match2, groups') <- r (RegConcat rs) post1 (begin && match1 == "") (replace groups1 (n-1) (Just match1))
    return (post, match1 ++ match2, groups')
  where
    replace l idx val =
      let (l1, _ : l2) = splitAt idx l
      in l1 ++ val : l2
r (RegConcat (RegRep _ 0 (Just 0) : rs)) s begin groups =
  r (RegConcat rs) s begin groups
r (RegConcat (RegRep r1 0 (Just m) : rs)) s begin groups =
  case result of
    Just x -> Just x
    Nothing -> r (RegConcat rs) s begin groups
  where result = r (RegConcat (r1 : RegRep r1 0 (Just (m-1)) : rs)) s begin groups
r (RegConcat (RegRep r1 n (Just m) : rs)) s begin groups =
  r (RegConcat (r1 : RegRep r1 (n-1) (Just (m-1)) : rs)) s begin groups
r (RegConcat (RegRep r1 0 Nothing : rs)) s begin groups =
  r (RegConcat (RegStar r1 : rs)) s begin groups
r (RegConcat (RegRep r1 n Nothing : rs)) s begin groups =
  r (RegConcat (r1 : RegRep r1 (n-1) Nothing : rs)) s begin groups
r (RegConcat (RegStar r1 : rs)) s begin groups =
  case r r1 s begin groups of
    Just (post, match, groups')
      | match /= "",
        Just (post', match', groups'') <- r' r1 (RegConcat rs) post groups' ->
          Just (post', match ++ match', groups'')
    Just (_, "", groups') ->
      r (RegConcat rs) s begin groups'
    Nothing ->
      r (RegConcat rs) s begin groups
    _ ->
      Nothing
r (RegConcat (_ : _)) _ _ _ = Nothing
r ritem s begin groups = r (RegConcat [ritem]) s begin groups

r' :: Regex -> Regex -> String -> CapGroups -> Maybe (String, String, CapGroups)
r' r1 rs s groups
  | Just (post, match, groups') <- r r1 s False groups,
    match /= "",
    Just (post', match', groups'') <- r' r1 rs post groups' =
      Just (post', match ++ match', groups'')
  | otherwise =
      r rs s False groups

-- vim: set sw=2 ts=2 et:
