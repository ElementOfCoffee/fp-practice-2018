module Task6 where

import Todo(todo)

data LinkedTree a = Void
                  | Node a (LinkedTree a) (LinkedTree a) (LinkedTree a)

find :: (Ord a) => LinkedTree a -> a -> Bool
find (Node v l r _) x | x < v = find l x
                      | x > v = find r x
                      | otherwise = True
find Void _ = False

insert :: (Ord a) => LinkedTree a -> a -> LinkedTree a
insert Void a = Node a Void Void Void
insert (Node v oP Void r) a
    | a < v = nP where 
        nP = Node v oP iN r
        iN = Node a nP Void Void
insert (Node v oP l Void) a
    | a > v = nP where 
        nP = Node v oP l iN 
        iN = Node a nP Void Void
insert x@(Node v oP l r ) a
    | a < v = Node v oP (insert l a) r
    | a > v = Node v oP l (insert r a)
    | otherwise = x

remove :: (Ord a) => LinkedTree a -> a -> LinkedTree a
remove Void _ = Void
remove (Node v p l r) a
    | a < v = Node v p (remove l a) r
    | a > v = Node v p l (remove r a)
    | otherwise = join l r where 
        join x Void = x
        join x (Node v p l r) = Node v p (join l x) r
