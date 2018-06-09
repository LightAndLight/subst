module Expr.Subst.Impl (Term, F, _B, _F, _Binder) where

import Control.Lens.Prism (Prism')

import Expr

type Term = Expr

type F = String

_B :: Prism' Term Int
_B = _BoundVar

_F :: Prism' Term F
_F = _FreeVar

_Binder :: Prism' Term Term
_Binder = _Abs
