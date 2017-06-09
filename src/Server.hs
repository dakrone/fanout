module Server ( Server
              , newServer
              , getName
              , getQueueSize) where

import Operation as Op
import Control.Concurrent.STM (STM(..), atomically)
import Control.Concurrent.STM.TChan

data Server =
  Server { _name :: String
         , _queue :: TChan (Op.Operation Int)
         } deriving (Eq)

newServer :: String -> IO Server
newServer s = do
  q <- atomically $ newTChan
  return Server { _name=s, _queue = q }

getName :: Server -> String
getName s = _name s

-- Execute next operation in the queue, returning a new Server with updated queue
executeNextOp :: Server -> IO ()
executeNextOp Server { _name=n, _queue=q } = do
  atomically $ do
    op <- readTChan q
    Op.execute op
