module Main where

import Data.List
import System.Directory
import System.Environment
import System.Exit

import Parser
import qualified Static as Static
import qualified Dynamic as Dynamic


runFile :: String -> Bool -> IO ()
runFile fname nflag = do
    source <- readFile fname
    prog <- case parseProgram fname source of
        Left err -> die (show err)
        Right prog -> return prog

    print prog
    print $ Static.ok prog
    input <- getContents
    print $ Dynamic.run prog input nflag


usage :: String
usage = intercalate "\n"
    [ "Usage:"
    , "  stack run sed [-n] <file.sed>  -- Interpret a sed program"
    , "  stack run test                 -- Run all files in ../tests" ]

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["sed", fname] -> runFile fname False
        ["sed", "-n", fname] -> runFile fname True

        ["test"] -> do
            let path = "tests/"
            fnames <- map (path ++) <$> listDirectory path
            let runner fname = runFile fname False
            sequence_ $ intersperse (putStrLn "") $ map runner fnames

        _ -> die usage
