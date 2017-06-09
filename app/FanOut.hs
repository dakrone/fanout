module Main where

import Server
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM (STM(..), atomically)
import Control.Concurrent.STM.TChan

oneSecond :: Int
oneSecond = 1000000

main :: IO ()
main = do
  chan <- atomically $ newTChan
  continue <- newLatch
  putStrLn "Starting readers.."
  _ <- forkIO $ readAndExecute chan continue
  _ <- forkIO $ readAndExecute chan continue
  putStrLn "Starting writer.."
  _ <- forkIO $ createOperations chan continue
  putStrLn "Pausing"
  threadDelay $ 2 * oneSecond
  putStrLn "Exiting"
  _ <- takeMVar continue
  putMVar continue False
  threadDelay oneSecond
