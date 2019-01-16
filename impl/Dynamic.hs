module Dynamic where
import AST

data Phase = NextCycle | ImplicitWrite | EndCycle | Exec | Final
  deriving (Show, Eq)

data State = State { phase :: Phase
                   , program :: Program
                   , inputLines :: [String]
                   , output :: String
                   , pattern :: String
                   , hold :: String
                   , appendQueue :: String
                   , lineNumber :: Int
                   , tFlag :: Bool
                   , nFlag :: Bool
                   , addrs :: [Int]
                   } deriving (Show)

-- Convert program to initial state of small-step semantics
startSmallStep :: Program -> String -> State
startSmallStep p i =
  smallStep state
  where state = State { phase = NextCycle
                      , program = p
                      , inputLines = lines i
                      , output = ""
                      , pattern = ""
                      , hold = ""
                      , appendQueue = ""
                      , lineNumber = 0
                      , tFlag = False
                      , nFlag = False
                      , addrs = [0]
                      }

-- Needs something for blocks
cmd :: Program -> [Int] -> Maybe Cmd
cmd (Program cmds) [x] = if length cmds > x
                         then Just (cmds !! x)
                         else Nothing
cmd _ _ = error "Unimplemented in function cmd"

incaddrs :: [Int] -> [Int]
incaddrs [x] = [x + 1]
incaddrs (x:xs) = x:(incaddrs xs)
incaddrs _ = error "Unimplemented in function incaddrs"

smallStep :: State -> State
smallStep state
  | phase state == NextCycle = if inputLines state == []
                               then state { phase = Final
                                          , output = output state ++ appendQueue state
                                          }
                               else smallStep state { phase = Exec
                                                    , output = output state ++ appendQueue state
                                                    , inputLines = (tail . inputLines) state
                                                    , pattern = (head . inputLines) state
                                                    , appendQueue = ""
                                                    , lineNumber = lineNumber state + 1
                                                    , addrs = [0]
                                                    }
  | phase state == Exec = smallStepExec state (cmd (program state) (addrs state))
  | phase state == EndCycle = smallStep $
                              if nFlag state
                              then state { phase = NextCycle }
                              else state { phase = NextCycle
                                         , output = (output state) ++ (pattern state)
                                         }
  | phase state == Final = state
  | otherwise = error "Unimplemented condition in smallStep"

smallStepExec :: State -> Maybe Cmd -> State
smallStepExec state Nothing = smallStep $ state { phase = EndCycle }
smallStepExec state (Just (Cmd NoAddr fun)) = smallStep result
  where
    result =
      case fun of
        Append string -> state { addrs = incaddrs (addrs state)
                               , appendQueue = appendQueue state ++ string
                               }
        _ -> error "Unimplemented function in smallStepExec"
smallStepExec _ (Just (Cmd _ _)) =
  error "Unimplemented address specification in smallStepExec"

testProg :: Program
testProg = Program [
  Cmd NoAddr (Append "hihi")
  ]
