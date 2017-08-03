module Ch18Bind where

import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

-- Prelude> bind (\x -> [x, 0]) [1,2,3]
-- [1,0,2,0,3,0]
