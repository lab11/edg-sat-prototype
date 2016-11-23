
module EDG.Classes.Fields where

import Control.Lens

class HasName s a | s -> a where
  name :: Lens' s a
