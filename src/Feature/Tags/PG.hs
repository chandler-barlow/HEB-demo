{-# LANGUAGE OverloadedStrings #-}

module Feature.Tags.PG
  ( addTag,
    Tag,
    tagById,
    tagsByImgId,
    tagByLabel,
  )
where

import Data.Maybe
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Only (..), close, connect, query)
import Feature.Tags.Types (Tag (..))
import Util.Config (pgConfig)

tagById :: Int -> IO [Tag]
tagById i = do
  c <- pgConfig >>= connect
  tags <- query c q [i]
  close c
  return tags
  where
    q = "SELECT * FROM tags WHERE id = ?"

addTag :: T.Text -> IO Tag
addTag l = do
  c <- pgConfig >>= connect
  tags <- query c q [T.unpack l]
  close c
  return $ head tags
  where
    q = "INSERT INTO tags (label) VALUES (?) RETURNING id, label"

tagByLabel :: T.Text -> IO [Tag]
tagByLabel l = do
  c <- pgConfig >>= connect
  tis <- query c q [T.unpack l]
  close c
  return tis
  where
    q = "SELECT * FROM tags WHERE label = ?"

tagsByImgId :: Int -> IO [Tag]
tagsByImgId i = do
  c <- pgConfig >>= connect
  tags <- query c q [i]
  close c
  return tags
  where
    q = "SELECT * FROM tags WHERE id IN (SELECT tag_id FROM tag_pairs WHERE image_id = ?)"
