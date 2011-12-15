-- a fork of Sebastiaan Visser's BSD3 fclabels
  
{-# LANGUAGE TypeOperators, TypeSynonymInstances, TemplateHaskell #-}

module Air.Data.Record.SimpleLabel
  (
  -- * Getter, setter and modifier types.
    Getter
  , Setter
  , Modifier

  -- * Label type.
  , Point
  , (:->) (Label)
  , label
  , get, set, mod
  , getM, setM, modM
  , (=:)

  -- * Derive labels using Template Haskell.
  , module Air.Data.Record.SimpleLabel.TH
  )
where

import Prelude hiding ((.), id, mod)
import Control.Applicative
import Control.Category
import Control.Monad.State hiding (get)
import Air.Data.Record.SimpleLabel.TH


type Getter   s x   = s -> x
type Setter   s x   = x -> s -> s
type Modifier s x = (x -> x) -> s -> s

data Point s x = Point
  { _get :: Getter s x
  , _set :: Setter s x
  }

_mod :: Point s x -> (x -> x) -> s -> s
_mod l f a = _set l (f (_get l a)) a

newtype (s :-> x) = Label { unLabel :: Point s x }


-- Create a label out of a getter and setter.

label :: Getter s x -> Setter s x -> s :-> x
label g s = Label (Point g s)

-- | Get the getter function from a label.

get :: (s :-> x) -> s -> x
get = _get . unLabel

-- | Get the setter function from a label.

set :: (s :-> x) -> x -> s -> s
set = _set . unLabel

-- | Get the modifier function from a label.

mod :: (s :-> x) -> (x -> x) -> s -> s
mod = _mod . unLabel

instance Category (:->) where
  id = Label (Point id const)
  (Label a) . (Label b) = Label (Point (_get a . _get b) (_mod b . _set a))

-- | Get a value out of state pointed to by the specified label.

getM :: MonadState s m => s :-> b -> m b
getM = gets . get

-- | Set a value somewhere in state pointed to by the specified label.

setM :: MonadState s m => s :-> b -> b -> m ()
setM l = modify . set l

-- | Alias for `setM' that reads like an assignment.

infixr 7 =:
(=:) :: MonadState s m => s :-> b -> b -> m ()
(=:) = setM

-- | Modify a value with a function somewhere in state pointed to by the
-- specified label.

modM :: MonadState s m => s :-> b -> (b -> b) -> m ()
modM l = modify . mod l
