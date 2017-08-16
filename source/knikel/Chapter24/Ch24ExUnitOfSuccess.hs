module Ch24ExUnitOfSuccess where

import Text.Trifecta

parzInt :: Parser Integer
parzInt = do
  i <- integer
  eof
  return i
