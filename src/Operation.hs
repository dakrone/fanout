module Operation ( Operation
                 , empty
                 , getId
                 , execute) where

data Operation a =
  Operation { _id       :: Int
            , _runnable :: Int -> a }

empty :: Int -> Operation Int
empty i = Operation { _id = i, _runnable = id }

getId :: Operation a -> Int
getId = _id

execute :: Operation a -> a
execute Operation { _runnable = f, _id = i } = f i
