module Rules where

data Rules = Rules [Rule]
  deriving (Eq, Show)

data Rule = Rule String [Judge] Judge
  deriving (Eq, Show)

data Judge = Judge [FItem]
  deriving (Eq, Show)

-- Formatting item
data FItem = Keyword String
           | Argument String
           | Command String
           | Math String
           | Code String
           | Parens [FItem]
  deriving (Eq, Show)
