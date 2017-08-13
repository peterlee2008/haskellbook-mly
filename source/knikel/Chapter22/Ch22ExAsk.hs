module Ch22ExAsk where

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

x = runReader ask True -- > True
