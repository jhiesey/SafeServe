module Air.Data.Monoid where

import Prelude hiding ((+))
import qualified Prelude as Prelude


import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Sequence

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

class Monoid a where
        mempty  :: a
        -- ^ Identity of 'mappend'
        mappend :: a -> a -> a
        -- ^ An associative operation
        mconcat :: [a] -> a

        -- ^ Fold a list using the monoid.
        -- For most types, the default definition for 'mconcat' will be
        -- used, but the function is included in the class definition so
        -- that an optimized version can be provided for specific types.

        mconcat = foldr mappend mempty


(+) :: (Monoid a) => a -> a -> a
(+) = mappend

infixl 6 +


-- Monoid instances.


instance Monoid Int where
  mempty = 0
  mappend = (Prelude.+)

instance Monoid Integer where
  mempty = 0
  mappend = (Prelude.+)

instance Monoid Double where
  mempty = 0
  mappend = (Prelude.+)

instance Monoid Float where
  mempty = 0
  mappend = (Prelude.+)


instance Monoid Int8 where
  mempty = 0
  mappend = (Prelude.+)

instance Monoid Int16 where
  mempty = 0
  mappend = (Prelude.+)
  
instance Monoid Int32 where
  mempty = 0
  mappend = (Prelude.+)
  
instance Monoid Int64 where
  mempty = 0
  mappend = (Prelude.+)
  
  
instance Monoid Word8 where
  mempty = 0
  mappend = (Prelude.+)
  
instance Monoid Word16 where
  mempty = 0
  mappend = (Prelude.+)
  
instance Monoid Word32 where
  mempty = 0
  mappend = (Prelude.+)
  
instance Monoid Word64 where
  mempty = 0
  mappend = (Prelude.+)


instance Monoid [a] where
        mempty  = []
        mappend = (++)

instance Monoid b => Monoid (a -> b) where
        mempty _ = mempty
        mappend f g x = f x `mappend` g x

instance Monoid () where
        -- Should it be strict?
        mempty        = ()
        _ `mappend` _ = ()
        mconcat _     = ()

instance (Monoid a, Monoid b) => Monoid (a,b) where
        mempty = (mempty, mempty)
        (a1,b1) `mappend` (a2,b2) =
                (a1 `mappend` a2, b1 `mappend` b2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
        mempty = (mempty, mempty, mempty)
        (a1,b1,c1) `mappend` (a2,b2,c2) =
                (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
        mempty = (mempty, mempty, mempty, mempty)
        (a1,b1,c1,d1) `mappend` (a2,b2,c2,d2) =
                (a1 `mappend` a2, b1 `mappend` b2,
                 c1 `mappend` c2, d1 `mappend` d2)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
                Monoid (a,b,c,d,e) where
        mempty = (mempty, mempty, mempty, mempty, mempty)
        (a1,b1,c1,d1,e1) `mappend` (a2,b2,c2,d2,e2) =
                (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2,
                 d1 `mappend` d2, e1 `mappend` e2)

instance Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  Just m `mappend` _ = Just m

instance Monoid (B.ByteString) where
  mempty = B.empty
  mappend = B.append

instance Monoid (LB.ByteString) where
  mempty = LB.empty
  mappend = LB.append

instance (Ord a) => Monoid (Map.Map a b) where
  mempty = Map.empty
  mappend = Map.union

instance (Ord a) => Monoid (Set.Set a) where
  mempty = Set.empty
  mappend = Set.union

instance Monoid (Sequence.Seq a) where
  mempty = Sequence.empty
  mappend = (Sequence.><)

