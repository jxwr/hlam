module Main where

import System.Environment
import Parse

evalFile :: String -> IO ()
evalFile filename = do
  s <- readFile filename
  mapM_ putStrLn $ map showResult (parseHlam filename s)
    where
      showResult (Left err) = show err
      showResult (Right result) = show result

main :: IO ()
main = do
  args <- getArgs
  mapM_ evalFile args
