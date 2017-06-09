module Server ( Server
              , newServer
              , getName
              , readAndExecute
              , newLatch
              , createOperations) where

import Operation as Op
import Control.Monad
import Control.Concurrent.MVar
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

-- -- Execute next operation in the queue, returning a new Server with updated queue
-- executeNextOp :: Server -> IO ()
-- executeNextOp Server { _name=n, _queue=q } = do
--   atomically $ do
--     op <- readTChan q
--     liftM Op.execute op

type Latch = MVar Bool

newLatch :: IO (MVar Bool)
newLatch = newMVar True

readAndExecute :: TChan (Op.Operation Int) -> Latch -> IO ()
readAndExecute chan latch = do
  op <- atomically $ readTChan chan
  putStrLn $ "got: " ++ show (Op.execute op)
  continue <- readMVar latch
  case continue of
    True -> readAndExecute chan latch
    False -> return ()

createOperations :: TChan (Op.Operation Int) -> Latch -> IO ()
createOperations chan latch =
  let op = (Op.empty 1) in do
    atomically $ writeTChan chan op
    continue <- readMVar latch
    case continue of
      True -> createOperations chan latch
      False -> return ()
