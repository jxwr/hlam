module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  mapM_ evalFile args

evalFile :: String -> IO ()
evalFile filename = do
  s <- readFile filename
  putStrLn s
