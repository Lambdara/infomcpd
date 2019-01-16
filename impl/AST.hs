module AST where


data Program = Program [Cmd]
  deriving (Show, Eq)

type Label = String

data Cmd = Cmd Addr Fun
  deriving (Show, Eq)

data Fun = Block [Cmd]
         | Label Label
         | Branch (Maybe Label)
         | To (Maybe Label)
         | Quit
         | LineNum
         | Delete
         | DeleteNL
         | Print
         | PrintNL
         | Next
         | NextAppend
         | Get
         | GetAppend
         | Hold
         | HoldAppend
         | Exchange
         | Append String
         | Insert String
         | Change String
         | Subst Regex String SFlags
         | Trans String String
  deriving (Show, Eq)

data Addr = NoAddr | Addr1 Bool BaseAddr | Addr2 Bool BaseAddr BaseAddr
  deriving (Show, Eq)

data BaseAddr = ALine Int | AEnd | ARegex Regex
  deriving (Show, Eq)

-- TODO: refine
type Regex = String

data SFlags = SFlags [SFlag]
  deriving (Show, Eq)

data SFlag = SNth Int | SGlob | SPrint
  deriving (Show, Eq)
