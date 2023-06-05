{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Feature.Images.Types (Metadata (..), Response (..), Image (..)) where

import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data Metadata = Metadata
  { metadata :: Image,
    tags :: [T.Text]
  }
  deriving (Generic, Show)

instance ToJSON Metadata

newtype Response = Response
  { items :: [Metadata]
  }
  deriving (Generic, Show)

instance ToJSON Response

data Image = Image
  { imageId :: Int,
    uri :: T.Text,
    date :: Day,
    time :: TimeOfDay,
    width :: Int,
    height :: Int,
    format :: T.Text
  }
  deriving (Generic, Show, FromRow)

instance ToJSON Image where
  toEncoding = genericToEncoding defaultOptions

instance Ord Image where
  compare a b = compare (imageId a) (imageId b)

instance Eq Image where
  (==) a b = imageId a == imageId b
