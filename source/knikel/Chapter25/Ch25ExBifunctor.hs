module Ch25ExBifunctor where

class Bifunctor p where

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (c -> d) -> p a c -> p a d
  second f = bimap id f

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g = first f . second g
  first f  (Deux a b) = Deux (f a) b
  second f (Deux a b) = Deux a (f b)

data Const a b = Const a

instance Bifunctor Const where
  bimap f g = first f . second g
  first f (Const a)  = Const $ f a
  second _ (Const a) = Const a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g = first f . second g
  first f (Drei a b c) = Drei a (f b) c
  second f (Drei a b c) = Drei a b (f c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f g = first f . second g
  first  f (SuperDrei a b) = SuperDrei a (f b)
  second _ (SuperDrei a b) = SuperDrei a b

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap f g = first f . second g
  first  _ (SemiDrei a) = SemiDrei a
  second _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadriceps a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g = first f . second g
  first f  (Quadriceps a b c d) = Quadriceps a b (f c) d
  second f (Quadriceps a b c d) = Quadriceps a b c (f d)

data MyEither a b =
  MyLeft a
  | MyRight b

instance Bifunctor MyEither where
  bimap f g = first f . second g

  first  f (MyLeft a) = MyLeft (f a)
  first  _ (MyRight b) = MyRight b

  second _ (MyLeft a) = MyLeft a
  second f (MyRight b) = MyRight (f b)
