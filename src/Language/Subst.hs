{-# language TypeFamilies, FlexibleContexts #-}
module Language.Subst where

import Control.Lens.Fold ((^?))
import Control.Lens.Plated (transformM, plate)
import Control.Lens.Review ((#))
import Control.Lens.Setter (over)
import Control.Monad ((<=<))
import Control.Monad.RevState (evalState, modify, get)

import Language.Subst.Indef

abstract :: F -> Term -> Term
abstract v = (_Binder #) . flip evalState 0 . transformM f
  where
    f =
      (\t -> maybe (pure t) (\_ -> t <$ modify (+1)) $ t ^? _Binder) <=<
      (\t ->
         case t ^? _F of
           Just v' | v == v' -> (_B #) <$> get
           _ -> pure t) <=<
      (pure . over _B (+1))

instantiate :: Term -> Term -> Maybe Term
instantiate f x = fun 0 <$> f ^? _Binder
  where
    fun n t =
      case t ^? _B of
        Nothing -> over plate (fun $ maybe n (\_ -> n+1) (t ^? _Binder)) t
        Just n'
          | n == n' -> x
          | otherwise -> _B # (n'-1)
