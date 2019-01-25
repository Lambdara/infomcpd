{-# LANGUAGE LambdaCase #-}
module Static(ok) where

import Control.Monad
import Data.Maybe

import AST


ok :: Program -> Bool
ok program = all ($ program) [labelCorrect, yCorrect, rCorrect]

yCorrect :: Program -> Bool
yCorrect (Program []) = True
yCorrect (Program (Cmd _ (Trans pat repl) : cmds)) =
    length pat == length repl && noDuplicates pat && yCorrect (Program cmds)
yCorrect (Program (Cmd _ (Block cmds1) : cmds2)) =
    yCorrect (Program cmds1) && yCorrect (Program cmds2)
yCorrect (Program (_ : cmds)) = yCorrect (Program cmds)

noDuplicates :: String -> Bool
noDuplicates "" = True
noDuplicates (c:cs) = notElem c cs && noDuplicates cs

labelCorrect :: Program -> Bool
labelCorrect s =
    case getLabels s of
        Nothing -> False
        Just l -> labelCorrect' s l

labelCorrect' :: Program -> [Label] -> Bool
labelCorrect' (Program []) _ = True
labelCorrect' (Program (Cmd _ (Branch (Just label)) : cmds)) labels =
    elem label labels && labelCorrect' (Program cmds) labels
labelCorrect' (Program (Cmd _ (To (Just label)) : cmds)) labels =
    elem label labels && labelCorrect' (Program cmds) labels
labelCorrect' (Program (Cmd _ (Block cmds1) : cmds2)) labels =
    labelCorrect' (Program cmds1) labels &&
    labelCorrect' (Program cmds2) labels
labelCorrect' (Program (_ : cmds)) labels = labelCorrect' (Program cmds) labels

-- Returns Just a list of labels if no errors occur
-- Returns Nothing if an error occurs
-- Currently the only possible error is a duplicate label
getLabels :: Program -> Maybe [Label]
getLabels (Program []) = Just []
getLabels (Program (Cmd _ (Label label) : cmds)) = do
    rest <- getLabels (Program cmds)
    if elem label rest
        then Nothing
        else return (label:rest)
getLabels (Program (Cmd _ (Block cmds1) : cmds2)) = getLabels (Program (cmds1 ++ cmds2))
getLabels (Program (_ : cmds)) = getLabels (Program cmds)

rCorrect :: Program -> Bool
rCorrect (Program []) = True
rCorrect (Program (Cmd (Addr1 _ a1) fun : cmds)) =
    aCorrect a1 && rCorrect (Program (Cmd NoAddr fun : cmds))
rCorrect (Program (Cmd (Addr2 _ a1 a2) fun : cmds)) =
    aCorrect a1 && aCorrect a2 && rCorrect (Program (Cmd NoAddr fun : cmds))
rCorrect (Program (Cmd NoAddr (Subst reg repl flags) : cmds)) =
    sCorrect reg repl flags && rCorrect (Program cmds)
rCorrect (Program (Cmd NoAddr _ : cmds)) =
    rCorrect (Program cmds)

aCorrect :: BaseAddr -> Bool
aCorrect (ALine _) = True
aCorrect AEnd = True
aCorrect (ARegex reg) = isJust (rCorrect' reg)

rCorrect' :: Regex -> Maybe Int
rCorrect' (RegConcat topregs) = go (reverse topregs)
  where
    go [] = Just 0
    go (RegBackref k : rs) = do
        n <- go rs
        guard (k <= n)
        return n
    go (RegGroup r1 _ : rs) = fmap succ $ go (r1 : rs)
    go (RegRep r1 k1 k2 : rs) =
        guard (k1 <= fromMaybe k1 k2) >> go (r1 : rs)
    go (RegAny : rs) = go rs
    go (RegChar _ : rs) = go rs
    go (RegClass _ _ : rs) = go rs
    go (RegStar r1 : rs) = go (r1 : rs)
    go (RegAnchorLeft : rs) = go rs
    go (RegAnchorRight : rs) = go rs
    go (RegConcat rs1 : rs) = go (rs1 ++ rs)
rCorrect' r = rCorrect' (RegConcat [r])

sCorrect :: Regex -> RegRepl -> SFlags -> Bool
sCorrect reg repl _flags = case rCorrect' reg of
    Just n -> replCorrect repl n
    Nothing -> False

replCorrect :: RegRepl -> Int -> Bool
replCorrect (RegRepl []) _n = True
replCorrect (RegRepl (RRBackref k : rs)) n = k <= n && replCorrect (RegRepl rs) n
replCorrect (RegRepl (RRChar _ : rs)) n = replCorrect (RegRepl rs) n

-- vim: sw=4 ts=4 et:
