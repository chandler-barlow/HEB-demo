{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Feature.TagPairs.Types (TagPair) where

import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data TagPair = TagPair
  { tagId :: Int,
    imageId :: Int
  }
  deriving (Generic, Show, FromRow, Ord, Eq)

instance ToJSON TagPair
