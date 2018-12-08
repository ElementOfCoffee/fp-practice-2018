module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show, Eq)

f1 (FourOf a _ _ _) = a
f2 (FourOf _ a _ _) = a
f3 (FourOf _ _ a _) = a
f4 (FourOf _ _ _ a) = a

instance Monad FourOf where
	(>>=) (FourOf a b c d ) f = FourOf (f1 (f a)) (f2 (f b)) (f3 (f c)) (f4 (f d))
	return a = FourOf a a a a

instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
    pure = return
    (<*>)(FourOf a1 b1 c1 d1) (FourOf a2 b2 c2 d2) = FourOf (a1 a2) (b1 b2) (c1 c2) (d1 d2)