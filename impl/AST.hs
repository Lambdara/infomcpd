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
         | Subst Regex RegRepl SFlags
         | Trans String String
  deriving (Show, Eq)

data Addr = NoAddr | Addr1 Bool BaseAddr | Addr2 Bool BaseAddr BaseAddr
  deriving (Show, Eq)

data BaseAddr = ALine Int | AEnd | ARegex Regex
  deriving (Show, Eq)

data Regex = RegChar Char
           | RegAny
           | RegBackref Int
           | RegClass Bool [RegClassItem]
           | RegAnchorLeft
           | RegAnchorRight
           | RegConcat [Regex]
           | RegGroup Regex Int
           | RegStar Regex
           | RegRep Regex Int (Maybe Int)
  deriving (Show, Eq)

newtype RegRepl = RegRepl [RegReplItem]
  deriving (Show, Eq)

data RegReplItem = RRChar Char | RRBackref Int
  deriving (Show, Eq)

data RegClassItem = RCChar Char | RCRange Char Char
  deriving (Show, Eq)

data SFlags = SFlags [SFlag]
  deriving (Show, Eq)

data SFlag = SNth Int | SGlob | SPrint
  deriving (Show, Eq)
