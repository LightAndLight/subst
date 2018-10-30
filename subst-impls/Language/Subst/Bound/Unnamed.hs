module Language.Subst.Bound.Unnamed
  (B, index)
where

import Control.Lens.Lens (Lens')

type B = Int

index :: Lens' B Int
index = id
