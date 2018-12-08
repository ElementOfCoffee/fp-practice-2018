module Task3_3 where

newtype PSet   a = PSet   {contains   :: (a -> Bool) }
newtype PSet'  a = PSet'  {contains'  :: (a -> Bool) }
newtype PSet'' a = PSet'' {contains'' :: (a -> Bool) }
-- сложение множеств
instance Monoid (PSet a) where
    mempty = PSet (\a -> False)
    mappend (PSet x) (PSet y) = PSet (\a -> (||) (x a) (y a))

--пересечение множеств
instance Monoid (PSet' a) where
    mempty = PSet' (\a -> True)
    mappend (PSet' x) (PSet' y) = PSet' (\a -> (&&) (x a) (y a))

-- симметрическая разность
instance Monoid (PSet'' a) where
    mempty = PSet'' (\a -> False)
    mappend (PSet'' x) (PSet'' y) = PSet'' (\a -> ((x a) && (not $ y a)) || ((not $ x a) && (y a)))

-- Версия функтора. 
-- Так как мы не знаем ничего о множестве b, то и релизовать полноценно функтор нельзя. 
instance Functor PSet where
    fmap f (PSet a) = PSet (\b -> False)
