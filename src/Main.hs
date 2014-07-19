module Main where

import System.Environment
import Parse

evalFile :: String -> IO ()
evalFile filename = do
  s <- readFile filename
  case parseHlam filename s of
    Left err -> putStrLn err
    Right stmts -> mapM_ (putStrLn . show) stmts

main :: IO ()
main = do
  args <- getArgs
  mapM_ evalFile args
