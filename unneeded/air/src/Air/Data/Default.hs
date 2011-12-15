module Air.Data.Default where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- BEGIN
-- copy from data.default

import Data.Ratio
import qualified Data.Set as S
import qualified Data.Map as M

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Time (Day(..), TimeOfDay, midnight, UTCTime(..), DiffTime, secondsToDiffTime)
-- | A class for types with a default value.
class Default a where
    -- | The default value for this type.
    def :: a

instance Default () where def = ()

instance Default (S.Set v) where def = S.empty
instance Default (M.Map k v) where def = M.empty

instance Default Int where def = 0
instance Default Integer where def = 0
instance Default Float where def = 0
instance Default Double where def = 0
instance (Integral a) => Default (Ratio a) where def = 0

instance Default (Maybe a) where def = Nothing
instance Default [a] where def = []

instance (Default r) => Default (e -> r) where def _ = def
instance (Default a) => Default (IO a) where def = return def

instance (Default a, Default b) => Default (a, b) where
  def = (def, def)

-- END



instance Default B.ByteString where
  def = B.empty

instance Default L.ByteString where
  def = L.empty


instance Default Int8 where def = 0
instance Default Int16 where def = 0
instance Default Int32 where def = 0
instance Default Int64 where def = 0
instance Default Word8 where def = 0
instance Default Word16 where def = 0
instance Default Word32 where def = 0
instance Default Word64 where def = 0

instance Default Bool where def = False


instance (Default a, Default b, Default c) => Default (a, b, c) where
  def = (def, def, def)


instance (Default a, Default b, Default c, Default d) => Default (a, b, c, d) where
  def = (def, def, def, def)
  
instance (Default a, Default b, Default c, Default d, Default e) => Default (a, b, c, d, e) where
  def = (def, def, def, def, def)

instance Default Day where
  def = ModifiedJulianDay def

instance Default DiffTime where
  def = secondsToDiffTime def

instance Default UTCTime where
  def = UTCTime def def

instance Default TimeOfDay where
  def = midnight