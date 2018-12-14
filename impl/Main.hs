module Main where

import Data.List
import System.Environment
import System.Exit

import Parser


usage :: String
usage = intercalate "\n"
    [ "Usage: stack run <file.sed>"
    , "Interprets the given sed program." ]

main :: IO ()
main = do
    args <- getArgs
    source <- case args of
        [fname] -> readFile fname
        _ -> die usage

    prog <- case parseProgram "<stdin>" source of
        Left err -> die (show err)
        Right prog -> return prog

    print prog
