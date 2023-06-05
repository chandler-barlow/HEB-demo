{-# LANGUAGE OverloadedStrings #-}

module Feature.Images.PG
  ( allImages,
    addImage,
    searchByTags,
    searchById,
  )
where

import Control.Monad (forM)
import Data.Functor ((<&>))
import Data.List (group, sort)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, connect, query)
import Feature.Images.Types (Image)

allImages :: IO [Image]
allImages = do
  conn <- connect pgConfig
  imgs <- query conn q ()
  close conn
  return imgs
  where
    q = "SELECT * FROM images"

-- maybe I should actually query for images by tag
searchByTags :: [T.Text] -> IO [Image]
searchByTags tags = do
  conn <- connect pgConfig
  res <- queryByTags conn fTags
  close conn
  return $ rmDupes res
  where
    queryByTags c ts = forM ts (queryByTag c) <&> mconcat
    queryByTag c t = query c q [t]
    q =
      "SELECT images.id as id, uri, date, time, width, height, format \
      \FROM tag_pairs \
      \JOIN tags on tags.id = tag_id \
      \JOIN images on images.id = image_id \
      \WHERE label LIKE ?"
    fTags = map (\t -> T.unpack $ mconcat ["%", t, "%"]) tags
    rmDupes = map head . group . sort

searchById :: Int -> IO [Image]
searchById i = do
  conn <- connect pgConfig
  imgs <- query conn q [i]
  close conn
  return imgs
  where
    q = "SELECT * FROM images WHERE id = ?"

addImage :: T.Text -> Int -> Int -> T.Text -> IO Image
addImage u w h f = do
  conn <- connect pgConfig
  r <- query conn q (T.unpack u, w, h, T.unpack f)
  close conn
  return $ head r
  where
    q =
      "INSERT INTO images (uri, width, height, format, date, time) \
      \ VALUES (?, ?, ?, ?, CURRENT_DATE, CURRENT_TIME) \
      \ RETURNING id, uri, date, time, width, height, format"
