{-# LANGUAGE OverloadedStrings #-}

module Feature.Tags.PG
  ( addTag,
    Tag,
    tagById,
    tagsByImgId,
  )
where

import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Feature.Tags.Types (Tag (..))

tagById :: Int -> IO [Tag]
tagById i = do
  c <- connect pgConfig
  tags <- query c q [i]
  close c
  return tags
  where
    q = "SELECT * FROM tags WHERE id = ?"

addTag :: T.Text -> IO Tag
addTag l = do
  c <- connect pgConfig
  tags <- query c q [T.unpack l]
  close c
  return $ head tags
  where
    q = "INSERT INTO tags (label) VALUES (?) RETURNING id, label"

tagsByImgId :: Int -> IO [Tag]
tagsByImgId i = do
  c <- connect pgConfig
  tags <- query c q [i]
  close c
  return tags
  where
    q = "SELECT * FROM tags WHERE id IN (SELECT tag_id FROM tag_pairs WHERE image_id = ?)"
