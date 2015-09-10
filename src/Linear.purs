module Linear where
import Prelude
import Data.Generic

type Word = Int
type Float = Number
type Int16 = Int
type Int32 = Int
type Word16 = Int
type Word32 = Int
type Bool = Boolean

data V2 a = V2 a a
data V3 a = V3 a a a
data V4 a = V4 a a a a

-- matrices are stored in column major order
type M22F = V2 V2F
type M23F = V3 V2F
type M24F = V4 V2F
type M32F = V2 V3F
type M33F = V3 V3F
type M34F = V4 V3F
type M42F = V2 V4F
type M43F = V3 V4F
type M44F = V4 V4F

type V2F = V2 Float
type V3F = V3 Float
type V4F = V4 Float
type V2I = V2 Int32
type V3I = V3 Int32
type V4I = V4 Int32
type V2U = V2 Word32
type V3U = V3 Word32
type V4U = V4 Word32
type V2B = V2 Bool
type V3B = V3 Bool
type V4B = V4 Bool

derive instance genericV2 :: (Generic a) => Generic (V2 a)
instance showV2 :: (Generic a) => Show (V2 a) where show = gShow
instance eqV2   :: (Generic a) => Eq (V2 a)   where eq = gEq
instance ordV2  :: (Generic a) => Ord (V2 a)  where compare = gCompare

derive instance genericV3 :: (Generic a) => Generic (V3 a)
instance showV3 :: (Generic a) => Show (V3 a) where show = gShow
instance eqV3   :: (Generic a) => Eq (V3 a)   where eq = gEq
instance ordV3  :: (Generic a) => Ord (V3 a)  where compare = gCompare

derive instance genericV4 :: (Generic a) => Generic (V4 a)
instance showV4 :: (Generic a) => Show (V4 a) where show = gShow
instance eqV4   :: (Generic a) => Eq (V4 a)   where eq = gEq
instance ordV4  :: (Generic a) => Ord (V4 a)  where compare = gCompare
