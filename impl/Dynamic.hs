module Dynamic(run) where

import Data.List

import AST


data Machine
    = NextCycle State
    | EndCycle State
    | Exec [DynCmd] IP State
    | Find Label [DynCmd] IP State
    | Match BaseAddr State Machine Machine
    | EnableAddr BaseAddr [DynCmd] IP State
    | Final String
  deriving (Show)

data State = State
        { sCode :: [DynCmd]
        , sInputLines :: [String]
        , sOutput :: String
        , sPat :: String
        , sHold :: String
        , sAq :: String
        , sLnnum :: Int
        , sTflag :: Bool
        , sNflag :: Bool
        , sAddrs :: [[Int]]
        }
  deriving (Show)

data DownArrow = DownArrow
  deriving (Show)

type DynCmd = Either DownArrow Cmd

type IP = [Int]

-- Run a program according to the dynamic semantics
run :: Program -> String -> Bool -> String
run (Program cmds) input nflag = transitionLoop (NextCycle initState)
  where
    initState = State
      { sCode = map Right cmds
      , sInputLines = lines input
      , sOutput = ""
      , sPat = ""
      , sHold = ""
      , sAq = ""
      , sLnnum = 0
      , sTflag = False
      , sNflag = nflag
      , sAddrs = []
      }

transitionLoop :: Machine -> String
transitionLoop (Final str) = str
transitionLoop machine = transitionLoop (smallStep machine)

smallStep :: Machine -> Machine
smallStep (NextCycle state)
  | null (sInputLines state) = Final (sOutput state ++ sAq state )
  | otherwise =
      let (line : rest) = sInputLines state
      in Exec (sCode state) [0]
            state { sOutput = sOutput state ++ sAq state
                  , sAq = ""
                  , sInputLines = rest
                  , sLnnum = sLnnum state + 1
                  , sPat = line
                  , sTflag = False
                  }
smallStep (EndCycle state) =
    NextCycle (implWrite state)
smallStep (Exec [] _ip state) = EndCycle state
smallStep (Exec (Left DownArrow : code) (_n : m : ip) state) =
    Exec code (m + 1 : ip) state
smallStep (Exec (Left DownArrow : _code) _ip _state) =
    error "Inconsistency: invalid IP in DownArrow"
smallStep (Exec (Right cmd : code) ip state) =
    execCmd cmd code ip state
smallStep (Find label code ip state) =
    execFind label code ip state
smallStep (Match (ALine n) state machine1 machine2) =
    if sLnnum state == n then machine1 else machine2
smallStep (Match AEnd state machine1 machine2) =
    if null (sInputLines state) then machine1 else machine2
smallStep (EnableAddr (ALine n) code ip state)
  | n <= sLnnum state = Exec code ip state
smallStep (EnableAddr _ code ip state) =
    Exec code ip state { sAddrs = ip : sAddrs state }
smallStep (Final _state) = error "Should not execute Final machine"

execCmd :: Cmd -> [DynCmd] -> IP -> State -> Machine
execCmd (Cmd NoAddr fun) code ip state =
    execFun fun code ip state
execCmd (Cmd (Addr1 True addr) fun) code ip state =
    Match addr state (Exec (toDynCmd fun : code) ip state)
                     (Exec code (incrIP ip) state)
execCmd (Cmd (Addr1 False addr) fun) code ip state =
    Match addr state (Exec code (incrIP ip) state)
                     (Exec (toDynCmd fun : code) ip state)
execCmd (Cmd (Addr2 True addr1 addr2) fun) code ip state
  | ip `notElem` sAddrs state =
      Match addr1 state (EnableAddr addr2 (toDynCmd fun : code) ip state)
                        (Exec code (incrIP ip) state)
  | otherwise =
      let l2 = sAddrs state \\ [ip]
      in Match addr2 state
               (Match addr1 state
                      (Exec (toDynCmd fun : code) ip state)
                      (Exec (toDynCmd fun : code) ip state { sAddrs = l2 }))
               (Exec (toDynCmd fun : code) ip state)
execCmd (Cmd (Addr2 False addr1 addr2) fun) code ip state
  | ip `notElem` sAddrs state =
      Match addr1 state (EnableAddr addr2 code (incrIP ip) state)
                        (Exec (toDynCmd fun : code) ip state)
  | otherwise =
      let l2 = sAddrs state \\ [ip]
      in Match addr2 state
               (Match addr1 state
                      (Exec code (incrIP ip) state)
                      (Exec code (incrIP ip) state { sAddrs = l2 }))
               (Exec code (incrIP ip) state)

execFun :: Fun -> [DynCmd] -> IP -> State -> Machine
execFun (Block cmds) code ip state =
    Exec (map Right cmds ++ [Left DownArrow] ++ code) (0 : ip) state
execFun (Append text) code ip state =
    Exec code (incrIP ip) state { sAq = sAq state ++ text ++ "\n" }
execFun (Branch (Just label)) _code _ip state =
    Find label (sCode state) [0] state
execFun (Branch Nothing) _code _ip state =
    EndCycle state
execFun Delete _code _ip state =
    NextCycle state
execFun DeleteNL _code _ip state
  | '\n' `notElem` sPat state = EndCycle state
  | otherwise =
      let (_line, rest) = splitLine (sPat state)
      in Exec (sCode state) [0] state { sPat = rest }
execFun Get code ip state =
    Exec code (incrIP ip) state { sPat = sHold state }
execFun GetAppend code ip state =
    Exec code (incrIP ip) state { sPat = sPat state ++ "\n" ++ sHold state }
execFun Hold code ip state =
    Exec code (incrIP ip) state { sHold = sPat state }
execFun HoldAppend code ip state =
    Exec code (incrIP ip) state { sHold = sHold state ++ "\n" ++ sPat state }
execFun (Insert text) code ip state =
    Exec code (incrIP ip) state { sOutput = sOutput state ++ text ++ "\n" }
execFun Next code ip state
  | null (sInputLines state) = EndCycle state
  | otherwise =
      let state' = implWrite state
          (line : rest) = sInputLines state
      in Exec code (incrIP ip)
              state' { sPat = line
                     , sInputLines = rest
                     , sLnnum = sLnnum state' + 1
                     , sTflag = False
                     }
execFun NextAppend code ip state
  | null (sInputLines state) = Final (sOutput state)
  | otherwise =
      let (line : rest) = sInputLines state
      in Exec code (incrIP ip)
              state { sPat = sPat state ++ "\n" ++ line
                    , sInputLines = rest
                    , sLnnum = sLnnum state + 1
                    , sTflag = False
                    }
execFun Print code ip state =
    Exec code (incrIP ip) state { sOutput = sOutput state ++ sPat state ++ "\n" }
execFun PrintNL code ip state
  | null (sPat state) = Exec code (incrIP ip) state { sOutput = sOutput state ++ "\n" }
  | otherwise =
      let (line, rest) = splitLine (sPat state)
      in Exec code (incrIP ip)
              state { sOutput = sOutput state ++ line ++ "\n"
                    , sPat = rest
                    }
execFun Quit _code _ip state =
    Final (sOutput (implWrite state))
execFun (To (Just label)) code ip state
  | sTflag state = Find label (sCode state) [0] state { sTflag = False }
  | otherwise = Exec code (incrIP ip) state
execFun (To Nothing) code ip state
  | sTflag state = EndCycle state { sTflag = False }
  | otherwise = Exec code (incrIP ip) state
execFun Exchange code ip state =
    Exec code (incrIP ip)
         state { sPat = sHold state
               , sHold = sPat state
               }
execFun (Label _label) code ip state =
    Exec code (incrIP ip) state
execFun LineNum code ip state =
    Exec code (incrIP ip)
         state { sOutput = sOutput state ++ show (sLnnum state) ++ "\n" }

execFind :: Label -> [DynCmd] -> IP -> State -> Machine
execFind _label [] _ip _state =
    error "Inconsistency: label not found in Find"
execFind label (Right (Cmd _ (Label label')) : code) ip state
  | label == label' = Exec code (incrIP ip) state
execFind label (Right (Cmd _ (Block cmds)) : code) ip state =
    Find label (map Right cmds ++ [Left DownArrow] ++ code) (0 : ip) state
execFind label (Left DownArrow : code) (_n : m : ip) state =
    Find label code (m + 1 : ip) state
execFind _label (Left DownArrow : _code) _ip _state =
    error "Inconsistency: invalid IP in Find DownArrow"
execFind label (_ : code) ip state =
    Find label code (incrIP ip) state

implWrite :: State -> State
implWrite state
  | sNflag state = state
  | otherwise = state { sOutput = sOutput state ++ sPat state ++ "\n" }

splitLine :: String -> (String, String)
splitLine = span (/= '\n')

incrIP :: IP -> IP
incrIP (n : ip) = (n + 1 : ip)
incrIP _ = undefined

toDynCmd :: Fun -> DynCmd
toDynCmd = Right . Cmd NoAddr
