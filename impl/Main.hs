module Main where

import Data.List
import System.Directory
import System.Environment
import System.Exit

import Parser


runFile :: String -> IO ()
runFile fname = do
    source <- readFile fname
    prog <- case parseProgram "<stdin>" source of
        Left err -> die (show err)
        Right prog -> return prog

    print prog


usage :: String
usage = intercalate "\n"
    [ "Usage:"
    , "  stack run sed <file.sed>  -- Interpret a sed program"
    , "  stack run test            -- Run all files in ../tests" ]

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["sed", fname] -> runFile fname

        ["test"] -> do
            let path = "../tests/"
            fnames <- listDirectory path
            mapM_ runFile $ map (path ++) fnames

        _ -> die usage
