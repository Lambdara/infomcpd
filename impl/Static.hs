module Static(ok) where
import AST

ok :: Program -> Bool
ok program = labelCorrect program && yCorrect program

-- Placeholder
yCorrect :: Program -> Bool
yCorrect (Program []) = True
yCorrect (Program (Trans _ pat repl:cmds)) = length pat == length repl &&
                                             noDuplicates pat &&
                                             yCorrect (Program cmds)
yCorrect (Program (Block _ cmds1:cmds2)) = yCorrect (Program cmds1) &&
                                           yCorrect (Program cmds2)
yCorrect (Program (_:cmds)) = yCorrect (Program cmds)

noDuplicates :: String -> Bool
noDuplicates "" = True
noDuplicates (c:cs) = notElem c cs &&
                      noDuplicates cs

labelCorrect :: Program -> Bool
labelCorrect s =
  case getLabels s of
    Nothing -> False
    Just l -> labelCorrect' s l

labelCorrect' :: Program -> [Label] -> Bool
labelCorrect' (Program []) _ = True
labelCorrect' (Program (Branch _ (Just label):cmds)) labels =
  elem label labels && labelCorrect' (Program cmds) labels
labelCorrect' (Program (To _ (Just label):cmds)) labels =
  elem label labels && labelCorrect' (Program cmds) labels
labelCorrect' (Program (Block _ cmds1:cmds2)) labels =
  labelCorrect' (Program cmds1) labels &&
  labelCorrect' (Program cmds2) labels
labelCorrect' (Program (_:cmds)) labels = labelCorrect' (Program cmds) labels

-- Returns Just a list of labels if no errors occur
-- Returns Nothing if an error occurs
-- Currently the only possible error is a duplicate label
getLabels :: Program -> Maybe [Label]
getLabels (Program []) = Just []
getLabels (Program (Label label:cmds)) = do
  rest <- getLabels (Program cmds)
  if elem label rest
    then Nothing
    else return (label:rest)
getLabels (Program (Block _ cmds1:cmds2)) = getLabels (Program (cmds1 ++ cmds2))
getLabels (Program (_:cmds)) = getLabels (Program cmds)
