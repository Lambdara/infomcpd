{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import System.Directory
import System.Exit
import System.IO
import System.Process

import Display
import Parser
import Process
import ToRules


mktemp :: IO FilePath
mktemp = init <$> readProcess "mktemp" ["-d"] ""

main :: IO ()
main = do
    prog <- parseProgram "stdin" <$> getContents >>= \case
        Right prog -> return prog
        Left err -> die (show err)

    let prog' = process prog
        rules = toRules prog'

    result <- concat <$> sequence
        [readFile "prologue.tex"
        ,return (display rules)
        ,readFile "epilogue.tex"]

    tempdir <- mktemp

    writeFile (tempdir ++ "/out.tex") result

    withFile "/dev/null" WriteMode $ \devnull -> do
        handle <- runProcess "pdflatex" ["out.tex"] (Just tempdir) Nothing Nothing (Just devnull) Nothing
        void $ waitForProcess handle

    copyFile (tempdir ++ "/out.pdf") "out.pdf"

    removeDirectoryRecursive tempdir
