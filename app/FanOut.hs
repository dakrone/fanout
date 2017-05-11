module Main where

import Server

main :: IO ()
main = do
  putStrLn $ "A server named: "
    ++ (getName n)
    ++ " with a queue size of "
    ++ (show (getQueueSize n))
  where n = newServer "foo"
