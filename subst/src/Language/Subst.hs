{-# language TypeFamilies, FlexibleContexts #-}
{-# language BangPatterns #-}
module Language.Subst where

import Control.Lens.Fold ((^?))
import Control.Lens.Plated (plate)
import Control.Lens.Review ((#))
import Control.Lens.Setter (over)

import Language.Subst.Indef

-- | Abstract over a particular free variable in a 'Term'
-- and wrap the result in a binder. Shifts bound variables that
-- come from "bigger" scopes.
abstract :: F -> Term -> Term
abstract v = (_Binder #) . fun 0
  where
    fun !n t =
      case t ^? _F of
        Just v'
          | v == v' -> _B # n
          | otherwise -> t
        Nothing ->
          case t ^? _B of
            Just x ->
              if x < n
              then t
              else _B # (x+1)
            Nothing ->
              case t ^? _Binder of
                Just{} -> over plate (fun $ n+1) t
                Nothing -> over plate (fun n) t

-- | Attempt to apply a value to a function
--
-- If @f@ is not a binder then @'instantiate' f x@ returns 'Nothing'
--
-- Otherwise, it returns 'Just', with @x@ substituted appropriately throughout
-- the body of @f@
--
-- Shifts bound variables from "bigger" scopes
instantiate :: Term -> Term -> Maybe Term
instantiate f x = fun 0 <$> f ^? _Binder
  where
    fun !n t =
      case t ^? _B of
        Just n' ->
          case compare n' n of
            LT -> t
            EQ -> x
            GT -> _B # (n'-1)
        Nothing ->
          case t ^? _Binder of
            Nothing -> over plate (fun n) t
            Just{} -> over plate (fun $ n+1) t
