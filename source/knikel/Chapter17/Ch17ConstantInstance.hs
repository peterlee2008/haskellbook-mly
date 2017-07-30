module Ch17ConstantInstance where

import Control.Applicative
import Data.Monoid

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant v) = Constant v

instance Monoid a => Applicative (Constant a) where
  pure x = Constant (mempty x)
  (<*>) (Constant a) (Constant a') = Constant $ a <> a'
