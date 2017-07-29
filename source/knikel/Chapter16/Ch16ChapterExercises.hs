{-# LANGUAGE FlexibleInstances #-}
module Ch16ChapterExercises where

-- Is valid functor?
-- 1. no, the kind is *, Functor requires the kind of * -> *
-- 2. yes
-- 3. yes
-- 4. I guess so, since f is (* -> *). The fmap will have to delegate to the fmap of the (* -> *) type
-- 5. nope, the D has a kind *.

-- Rearrange
-- 1.
data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

-- 2.
data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following

-- 1.
data Quant a b =
  Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

-- 2.
data K a b = K a deriving (Eq, Show)

instance Functor (K m) where
  fmap _ (K a) = K a

-- 3.
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

-- 4.
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b


-- 5.
data LiftItOut f a = LiftItOut (f a)

instance Functor fa => Functor (LiftItOut fa) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa
-- we delegate the fmap to the nested type

-- 6.
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- Num b => Parappa Maybe Maybe b
-- fmap (+1) $ DaWrappa (Just 1) Nothing

-- 7.
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8.
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
-- g o a types are part of the structure, therefore you can't touch them

-- 9.
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) $ fmap f rest


-- 10.
data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a a' a'') = MoreGoats (fmap f a) (fmap f a') (fmap f a'')

-- 11.
-- I'm not sure I get the (Read g) part, despite I managed to get it working.
data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print a b) = Print a (f b)
  fmap f (Read g) = Read (fmap f g)
