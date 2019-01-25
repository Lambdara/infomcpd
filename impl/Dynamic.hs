module Dynamic(run) where

import Data.List
import Data.Maybe

import AST
import Regex


data Machine
    = NextCycle State
    | EndCycle State
    | Exec [DynCmd] IP State
    | Find Label [DynCmd] IP State
    | Match BaseAddr State Machine Machine
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
        , sAddrs :: [Either [Int] [Int]]
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
smallStep (Match (ARegex reg) state machine1 machine2) =
    case regex reg (sPat state) of
        Just (_pre, _post, _groups) -> machine1
        Nothing -> machine2
smallStep (Final _state) = error "Should not execute Final machine"

execCmd :: Cmd -> [DynCmd] -> IP -> State -> Machine
execCmd (Cmd NoAddr (Change text)) code ip state =
    let l = [Cmd NoAddr (Insert text), Cmd NoAddr Delete]
    in Exec (Right (Cmd NoAddr (Block l)) : code) ip state
execCmd (Cmd (Addr1 pol a1) (Change text)) code ip state =
    let l = [Cmd NoAddr (Insert text), Cmd NoAddr Delete]
    in Exec (Right (Cmd (Addr1 pol a1) (Block l)) : code) ip state
execCmd (Cmd (Addr2 True a1 a2) (Change text)) code ip state =
    case addr2match a1 a2 ip state of
        (state', False, _atend) -> 
            Exec code (incrIP ip) state'
        (state', True, False) ->
            Exec (Right (Cmd NoAddr Delete) : code) ip state'
        (state', True, True) ->
            let l = [Cmd NoAddr (Insert text), Cmd NoAddr Delete]
            in Exec (Right (Cmd NoAddr (Block l)) : code) ip state'
execCmd (Cmd (Addr2 False a1 a2) (Change text)) code ip state =
    case addr2match a1 a2 ip state of
        (state', False, _atend) ->
            let l = [Cmd NoAddr (Insert text), Cmd NoAddr Delete]
            in Exec (Right (Cmd NoAddr (Block l)) : code) ip state'
        (state', True, _atend) ->
            Exec code (incrIP ip) state'

execCmd (Cmd (Addr1 True addr) fun) code ip state =
    Match addr state (Exec (toDynCmd fun : code) ip state)
                     (Exec code (incrIP ip) state)
execCmd (Cmd (Addr1 False addr) fun) code ip state =
    Match addr state (Exec code (incrIP ip) state)
                     (Exec (toDynCmd fun : code) ip state)
execCmd (Cmd (Addr2 pol addr1 addr2) fun) code ip state =
    let (state', exec, _atend) = addr2match addr1 addr2 ip state
    in if (exec && pol) || (not exec && not pol)
        then Exec (toDynCmd fun : code) ip state'
        else Exec code (incrIP ip) state'

execCmd (Cmd NoAddr fun) code ip state =
    execFun fun code ip state

execFun :: Fun -> [DynCmd] -> IP -> State -> Machine
execFun (Block cmds) code ip state =
    Exec (map Right cmds ++ [Left DownArrow] ++ code) (0 : ip) state
execFun (Append text) code ip state =
    Exec code (incrIP ip) state { sAq = sAq state ++ text ++ "\n" }
execFun (Branch (Just label)) _code _ip state =
    Find label (sCode state) [0] state
execFun (Branch Nothing) _code _ip state =
    EndCycle state
execFun (Change _) _ _ _ =
    error "Inconsistency: unexpected c in execFun"
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
execFun (Subst reg repl flags) code ip state
  | sfPrint flags = case subst reg repl flags { sfPrint = False } (sPat state) of
      Just res -> Exec code (incrIP ip)
                       state { sPat = res, sTflag = True, sOutput = sOutput state ++ res ++ "\n" }
      Nothing -> Exec code (incrIP ip) state
  | otherwise = case subst reg repl flags (sPat state) of
      Just res -> Exec code (incrIP ip) state { sPat = res, sTflag = True }
      Nothing -> Exec code (incrIP ip) state
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
execFun (Trans pat repl) code ip state =
    Exec code (incrIP ip) state { sPat = translate pat repl (sPat state) }
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
splitLine = fmap tail . span (/= '\n')

incrIP :: IP -> IP
incrIP (n : ip) = (n + 1 : ip)
incrIP _ = undefined

toDynCmd :: Fun -> DynCmd
toDynCmd = Right . Cmd NoAddr

addr2match :: BaseAddr -> BaseAddr -> IP -> State -> (State, Bool, Bool)
addr2match (ALine n) (ALine m) _ip state =
    (state, n <= sLnnum state && sLnnum state <= m, sLnnum state == m)
addr2match (ALine n) AEnd _ip state =
    (state, n <= sLnnum state, null (sInputLines state))
addr2match (ALine n) (ARegex r2) ip state =
    let enab = Right ip `elem` sAddrs state
        disab = Left ip `elem` sAddrs state
        regres = regex r2 (sPat state)
    in case (enab, disab, regres) of
        (True, _, Just (_pre, _post, _groups)) -> 
            (state { sAddrs = Left ip : sAddrs state }, True, True)
        (True, _, Nothing) ->
            (state, True, False)
        (_, True, _) ->
            (state, False, False)
        (False, False, _) | n <= sLnnum state ->
            (state { sAddrs = Right ip : sAddrs state }, True, False)
        (False, False, _) | sLnnum state < n ->
            (state, False, False)
        _ -> undefined
addr2match AEnd (ALine m) _ip state =
    (state, null (sInputLines state), null (sInputLines state) && m <= sLnnum state)
addr2match AEnd AEnd _ip state =
    (state, null (sInputLines state), null (sInputLines state))
addr2match AEnd (ARegex _r2) _ip state =
    (state, null (sInputLines state), False)
addr2match (ARegex r1) (ALine m) ip state =
    if Right ip `elem` sAddrs state
    then if m < sLnnum state
         then (state { sAddrs = sAddrs state \\ [Right ip] }, False, False)
         else (state, True, sLnnum state == m)
    else case regex r1 (sPat state) of
            Just (_pre, _post, _groups) ->
                (state { sAddrs = Right ip : sAddrs state }, True, False)
            Nothing ->
                (state, False, False)
addr2match (ARegex r1) AEnd ip state =
    if Right ip `elem` sAddrs state
    then (state, True, null (sInputLines state))
    else case regex r1 (sPat state) of
            Just (_pre, _post, _groups) ->
                (state { sAddrs = Right ip : sAddrs state }, True, False)
            Nothing ->
                (state, False, False)
addr2match (ARegex r1) (ARegex r2) ip state =
    if Right ip `elem` sAddrs state
    then case regex r2 (sPat state) of
            Just (_pre, _post, _groups) ->
                (state { sAddrs = sAddrs state \\ [Right ip] }, True, True)
            Nothing ->
                (state, True, False)
    else case regex r1 (sPat state) of
            Just (_pre, _post, _groups) ->
                (state { sAddrs = Right ip : sAddrs state }, True, False)
            Nothing ->
                (state, False, False)

subst :: Regex -> RegRepl -> SFlags -> String -> Maybe String
subst reg repl flags str
  | flags `elem` [SFlags Nothing False False, SFlags (Just 1) False False] =
      case subst1 reg repl str of
          Just (pre, res, post) -> Just (pre ++ res ++ post)
          Nothing -> Nothing
  | SFlags (Just n) False False <- flags,
    n >= 2 =
      case regex reg str of
          Just (pre, post, (match : _groups)) ->
              case subst reg repl (SFlags (Just (n - 1)) False False) post of
                  Just res -> Just (pre ++ fromJust match ++ res)
                  Nothing -> Nothing
          Just (_, _, []) ->
              error "Inconsistency: empty last tuple componenent from regex"
          Nothing ->
              Nothing
  | flags == SFlags Nothing True False =
      case subst1 reg repl str of
          Just (pre, res, post) -> case subst reg repl flags post of
              Just res' -> Just (pre ++ res ++ res')
              Nothing -> Just (pre ++ res ++ post)
          Nothing -> Nothing
  | otherwise = undefined

subst1 :: Regex -> RegRepl -> String -> Maybe (String, String, String)
subst1 reg repl str = case regex reg str of
    Just (pre, post, groups) -> Just (pre, srepl repl groups, post)
    Nothing -> Nothing

srepl :: RegRepl -> [Maybe String] -> String
srepl (RegRepl []) _groups = ""
srepl (RegRepl (RRChar c : rs)) groups = c : srepl (RegRepl rs) groups
srepl (RegRepl (RRBackref n : rs)) groups = fromMaybe "" (groups !! n) ++ srepl (RegRepl rs) groups

translate :: String -> String -> String -> String
translate "" "" str = str
translate (c : pat) (d : repl) str = translate pat repl (replc c d str)

replc :: Char -> Char -> String -> String
replc _c _d "" = ""
replc c d (c2 : s)
  | c == c2 = d : replc c d s
  | otherwise = c2 : replc c d s
