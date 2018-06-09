{-# language DeriveGeneric, TemplateHaskell #-}
module Term where

import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import GHC.Generics (Generic)

data Term
  = FreeVar String
  | BoundVar Int
  | App Term Term
  | Abs Term
  | Int Int
  deriving (Generic, Show)

makePrisms ''Term

instance Plated Term where
  plate = gplate

-----------------------------------
-- Language.Subst implementation --
-----------------------------------

type F = String

_B :: Prism' Term Int
_B = _BoundVar

_F :: Prism' Term F
_F = _FreeVar

_Binder :: Prism' Term Term
_Binder = _Abs
