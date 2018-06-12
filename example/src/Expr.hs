{-# language DeriveGeneric, TemplateHaskell #-}
module Expr where

import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import GHC.Generics (Generic)

data Expr
  = FreeVar String
  | BoundVar Int
  | App Expr Expr
  | Abs Expr
  | Int Int
  deriving (Generic, Show)

makePrisms ''Expr

instance Plated Expr where
  plate = gplate
  {-# inline plate #-}
