{-# language TypeFamilies, FlexibleContexts #-}
module Language.Subst where

import Control.Lens.Fold ((^?))
import Control.Lens.Plated (transform)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((%~))

import Language.Subst.Indef

abstract :: F -> Term -> Term
abstract v = (_Lam #) . transform f
  where
    f =
      (\t ->
         case t ^? _F of
           Nothing -> t
           Just v'
             | v == v' -> _B # 0
             | otherwise -> t) .
      (_B %~ (+1))

instantiate :: Term -> Term -> Term
instantiate f x =
  case f ^? _Lam of
    Nothing -> f
    Just body -> transform fun body
  where
    fun t =
      case t ^? _B of
        Nothing -> t
        Just n
          | n == 0 -> x
          | otherwise -> _B # (n-1)
