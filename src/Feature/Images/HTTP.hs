{-# LANGUAGE OverloadedStrings #-}

module Feature.Images.HTTP (getImages, getImageById, uploadImage) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Feature.Images.PG (addImage, allImages, searchById, searchByTags)
import Feature.Images.Requests (uploadImageToS3)
import Feature.Images.Types (Image (..), Metadata (..), Response (..))
import Feature.TagPairs.PG (addTagPair)
import Feature.Tags.PG
import Feature.Tags.Types (Tag (..))
import Network.HTTP.Types (status200)
import Network.Wai.Parse (fileContent, fileContentType, fileName)
import Web.Scotty
  ( ScottyM,
    files,
    get,
    json,
    param,
    post,
    rescue,
    status,
    text,
  )

getMeta :: [Image] -> IO [Metadata]
getMeta imgs = forM imgs $ \img -> do
  tags <- tagsByImgId $ imageId img
  return Metadata {metadata = img, tags = map label tags}

toList :: T.Text -> [T.Text]
toList s = T.splitOn (T.pack ",") $ T.filter (/= '\"') s

-- Routes
getImages :: ScottyM ()
getImages = get "/images" $ do
  tags <- param "objects" `rescue` (\_ -> return ("" :: T.Text))
  imgs <-
    let images = case tags of
          "" -> allImages >>= getMeta
          _ -> searchByTags (toList tags) >>= getMeta
     in liftIO images
  status status200
  json Response {items = imgs}

getImageById :: ScottyM ()
getImageById = get "/images/:id" $ do
  i <- param "id"
  imgs <- liftIO $ searchById i >>= getMeta
  status status200
  json $ Response {items = imgs}

po :: Show a => a -> IO ()
po v = do
  print v
  return ()

addTagIfNotExists :: T.Text -> IO Int
addTagIfNotExists t = do
  ts <- tagByLabel t
  case ts of
    [] -> addTag t >>= return . tagId
    ts -> return . tagId . head $ ts

-- tp is file type btw
uploadImage :: ScottyM ()
uploadImage = post "/images" $ do
  fs <- files
  _ <- liftIO (po fs)
  let tp = TL.pack . show . fileContentType . snd . head $ fs
  let fname = TL.toStrict . fst . head $ fs
  let fcontent = B.toStrict . fileContent . snd . head $ fs
  uri <- liftIO $ uploadImageToS3 fname fcontent
  i <- liftIO . addImage uri $ TL.toStrict tp
  tags <- param "objects" `rescue` (\_ -> return ("" :: T.Text))
  case tags of
    "" -> return ()
    _ -> do
      let ts = toList tags
      tis <- liftIO $ forM ts addTagIfNotExists
      liftIO $ forM_ tis $ \ti -> addTagPair (imageId i) ti
  -- autoTag <- param "autoTag" `rescue` (\_ -> return False)
  status status200
  text $ TL.pack . T.unpack $ tags
