{-# language TypeFamilies, FlexibleContexts #-}
{-# language BangPatterns #-}
module Language.Subst where

import Control.Lens.Fold ((^?))
import Control.Lens.Plated (plate)
import Control.Lens.Review ((#))
import Control.Lens.Setter (over)

import Language.Subst.Indef

abstract :: F -> Term -> Term
abstract v = (_Binder #) . fun 0
  where
    fun !n t =
      case t ^? _F of
        Just v'
          | v == v' -> _B # n
          | otherwise -> t
        _ -> over plate (fun $ maybe n (\_ -> n+1) (t ^? _Binder)) t

instantiate :: Term -> Term -> Maybe Term
instantiate f x = fun 0 <$> f ^? _Binder
  where
    fun !n t =
      case t ^? _B of
        Nothing -> over plate (fun $ maybe n (\_ -> n+1) (t ^? _Binder)) t
        Just n'
          | n == n' -> x
          | otherwise -> _B # (n'-1)
