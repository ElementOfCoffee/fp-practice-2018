module Task7 where

import Todo(todo)

data Deque a = Deque [a] [a]

empty :: Deque a
empty = Deque [] []

pushFront :: Deque a -> a -> Deque a
pushFront (Deque i o) x = Deque (x:i) o

popFront :: Deque a -> (a, Deque a)
popFront (Deque [] []) = error "Empty queue"
popFront (Deque (ih:it) o) = (ih, Deque it o)
popFront (Deque [] o) = popFront $ Deque (toHalve $ reverse o) (toHalve o)

pushBack :: Deque a -> a -> Deque a
pushBack (Deque i o) x = Deque i (x:o)

popBack :: Deque a -> (a, Deque a)
popBack (Deque [] []) = error "Empty deque"
popBack (Deque i (oe:ot)) = (oe, Deque i ot)
popBack (Deque i []) = popBack $ Deque (toHalve i) (toHalve $ reverse i)

toHalve :: [a] -> [a]
toHalve list  = take (fromInteger $ floor $ (toRational $ length list)/2) list
                      


