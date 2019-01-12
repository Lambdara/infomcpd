{-# LANGUAGE LambdaCase #-}
module Main where

import System.Exit

import Parser


main :: IO ()
main = do
    parseProgram "stdin" <$> getContents >>= \case
        Right prog -> print prog
        Left err -> die (show err)
