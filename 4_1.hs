module Task4_1 where

data FunMonad a = FunMonad { fun :: String -> a }

instance Functor FunMonad where
    fmap f (FunMonad fun) = FunMonad (f . fun)

instance Applicative FunMonad where
    pure a = FunMonad (\x -> a)
    (<*>) (FunMonad f1) (FunMonad f2) = FunMonad (\x -> f1 x $ f2 x)

instance Monad FunMonad where
    return a = FunMonad(\x -> a)
    (>>=) (FunMonad fm) f = FunMonad (\x -> (fun . f . fm $ x) $ x)
    


