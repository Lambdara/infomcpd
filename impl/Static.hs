module Static where
import AST

ok :: Program -> Bool
ok program = labelCorrect program && yCorrect program

-- Placeholder
yCorrect :: Program -> Bool
yCorrect = const True

labelCorrect :: Program -> Bool
labelCorrect s = labelCorrect' s l
  where l = getLabels s

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

getLabels :: Program -> [Label]
getLabels (Program []) = []
getLabels (Program (Label label:cmds)) = label:getLabels (Program cmds)
getLabels (Program (Block _ cmds1:cmds2)) = getLabels (Program cmds1) ++ getLabels (Program cmds2)
getLabels (Program (_:cmds)) = getLabels (Program cmds)
