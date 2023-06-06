{-# LANGUAGE OverloadedStrings #-}

module Feature.TagPairs.PG
  ( addTagPair,
    TagPair,
    tagPairByImgId,
    tagPairByTagId,
  )
where

import Database.PostgreSQL.Simple
  ( close,
    connect,
    execute,
    query,
  )
import Feature.TagPairs.Types (TagPair)
import Util.Config (pgConfig)

tagPairByImgId :: Int -> IO [TagPair]
tagPairByImgId i = do
  conn <- pgConfig >>= connect
  tagPairs <- query conn q [i]
  close conn
  return tagPairs
  where
    q = "SELECT * FROM tag_pairs WHERE image_id = ?"

tagPairByTagId :: Int -> IO [TagPair]
tagPairByTagId i = do
  conn <- pgConfig >>= connect
  tagPairs <- query conn q [i]
  close conn
  return tagPairs
  where
    q = "SELECT * FROM tag_pairs WHERE tag_id = ?"

addTagPair :: Int -> Int -> IO ()
addTagPair imgId tagId = do
  conn <- pgConfig >>= connect
  _ <- execute conn q (imgId, tagId)
  close conn
  return ()
  where
    q = "INSERT INTO tag_pairs (image_id, tag_id) VALUES (?, ?)"
