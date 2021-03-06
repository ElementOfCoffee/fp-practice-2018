module Task5_2 where

import Todo(todo)
import Data.Ratio

data Stream a = Cons {
                    shead :: a,
                    stail :: Stream a
                }

srepeat :: a -> Stream a
srepeat x =
    let rec = Cons x rec in
    rec

generate :: a -> (a -> a) -> Stream a
generate x f =
    Cons x $ generate (f x) f

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

diag (Cons h t) = Cons (shead h) $ diag (stail <$> t)
sflatten = diag

instance Applicative Stream where
    pure x = srepeat x
    f <*> x = do { f' <- f ; x' <- x ; return $ f' x' }

instance Monad Stream where
    return x = srepeat x
    ss >>= f = sflatten (f <$> ss)

sinPrecisions :: Double -> Stream Double
sinPrecisions x = r x 0 where 
    r x n = Cons (sinPrecisions' x n) (r x (n+1))

sinPrecisions' :: Double -> Int -> Double
sinPrecisions' x 0 = x
sinPrecisions' x n = sinPrecisions' x (n-1) + (-1)^n * x^(2*n + 1) /
                    (fromIntegral $ product [1..(2 * n + 1)])

ePrecisions :: Stream Rational
ePrecisions = r 0 where 
    r n = Cons (expFun n) (r (n+1))

expFun 0 = 2
expFun n = expFun (n-1) + (2*n + 2) / (product [1..(2*n + 1)])

