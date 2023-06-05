{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Feature.Tags.Types (Tag (..)) where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data Tag = Tag
  { tagId :: Int,
    label :: T.Text
  }
  deriving (Generic, Show, FromRow, Ord, Eq)

instance ToJSON Tag
