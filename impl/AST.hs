module AST where


data Program = Program [Cmd]
  deriving (Show, Eq)

type Label = String

data Cmd = Block Addr2 [Cmd]
         | Label Label
         | Branch Addr2 (Maybe Label)
         | To Addr2 (Maybe Label)
         | Quit Addr1
         | LineNum Addr1
         | Delete Addr2
         | DeleteNL Addr2
         | Print Addr2
         | PrintNL Addr2
         | Next Addr2
         | NextAppend Addr2
         | Get Addr2
         | GetAppend Addr2
         | Hold Addr2
         | HoldAppend Addr2
         | Exchange Addr2
         | Append Addr1 String
         | Insert Addr1 String
         | Change Addr2 String
         | Subst Addr2 Regex String SFlags
         | Trans Addr2 String String
  deriving (Show, Eq)

data Addr2 = NoAddr2
           | Addr2_1 Bool BaseAddr
           | Addr2 Bool (BaseAddr, BaseAddr)
  deriving (Show, Eq)

data Addr1 = NoAddr1
           | Addr1 Bool BaseAddr
  deriving (Show, Eq)

data BaseAddr = ALine Int | AEnd | ARegex Regex
  deriving (Show, Eq)

-- TODO: refine
type Regex = String

data SFlags = SFlags [SFlag]
  deriving (Show, Eq)

data SFlag = SNth Int | SGlob | SPrint
  deriving (Show, Eq)
