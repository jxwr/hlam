module Main where

import System.Environment
import Parse

evalFile :: String -> IO ()
evalFile filename = do
  s <- readFile filename
  case parseHlam filename s of
    Left err -> putStrLn err
    Right stmts -> do
      mapM_ putStrLn $ texts stmts
    where
      texts [] = []
      texts (x:xs) = 
          case x of
            Left err -> ("FAIL: " ++ err) : texts xs
            Right stmt -> ("PASS: " ++ show stmt) : texts xs

main :: IO ()
main = do
  args <- getArgs
  mapM_ evalFile args
