module Server ( Server
              , newServer
              , getName
              , getQueueSize) where

import Operation as Op
import Data.Sequence as Seq

data Server =
  Server { _name :: String
         , _queue :: Seq.Seq Op.Operation
         } deriving (Eq, Ord, Show)

newServer :: String -> Server
newServer s = Server { _name=s, _queue=Seq.empty }

getName :: Server -> String
getName s = _name s

getQueueSize :: Server -> Int
getQueueSize s = Seq.length $ _queue s

-- Execute next operation in the queue, returning a new Server with updated queue
executeNextOp :: Server -> Server
executeNextOp Server { _name=n, _queue=q } =
  -- execute $ Seq.take 1 q
  Server { _name=n, _queue=(Seq.drop 1 q) }
