module Pirec.Unify.Renaming (Renaming, rename, lift, invert) where

import Relude hiding (lift)

import Control.Effect hiding (lift)
import Control.Effect.Error
import Optics

import Pirec.Value (Spine)
import Pirec.Value qualified as V
import Pirec.Var.Level (Level (..))

import Pirec.Effect.Meta
import Pirec.Eval
import Pirec.Unify.Error

data Renaming = Renaming Level (IntMap Level)
  deriving stock (Show)

rename :: Level -> Renaming -> Maybe Level
rename (Level n) (Renaming _ m) = m ^. at n

lift :: Level -> Renaming -> Renaming
lift (Level n) (Renaming lvl m) = Renaming (lvl + 1) (m & at n ?~ lvl)

invert :: Effs [MetaLookup, Throw UnifyError] m => Spine -> m Renaming
invert = go
 where
  go = \case
    V.Nil -> pure $ Renaming 0 mempty
    V.App _ spine t -> do
      Renaming lvl m <- go spine
      forceValue t >>= \case
        V.Neut (V.Rigid lx@(Level n)) V.Nil
          | Just _ <- m ^. at n -> throw $ Nonlinear lx
          | otherwise -> pure $ Renaming (lvl + 1) (m & at n ?~ lvl)
        t -> throw $ Nonvariable t
    _ -> throw NonInvertable
