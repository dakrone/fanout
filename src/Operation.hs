module Operation ( Operation
                 , empty
                 , getId
                 , execute) where

class Runnable a where
  run :: a -> Int -> ()

data Noop = Noop deriving (Eq, Ord, Show)

instance Runnable Noop where
  run _ _ = ()

data Operation =
  Operation { _id       :: Int
            , _runnable :: Noop }
               deriving (Eq, Ord, Show)

empty :: Int -> Operation
empty i = Operation { _id = i, _runnable = Noop }

getId :: Operation -> Int
getId = _id

execute :: Operation -> ()
execute Operation { _runnable = f, _id = i } = run f i
