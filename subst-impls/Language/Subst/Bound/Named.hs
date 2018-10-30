module Language.Subst.Bound.Named
  (B, Named(..), index)
where

import Control.Lens.Lens (Lens', lens)
import Language.Subst.Free.Indef

type B = Named

data Named = Named F Int
  deriving Show

instance Eq B where
  a == b = a ^. index == b ^. index

index :: Lens' B Int
index =
  lens
    (\(Named _ i) -> i)
    (\i (Named f _) -> Named f i)
