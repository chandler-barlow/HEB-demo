{-# LANGUAGE OverloadedStrings #-}

module Feature.Images.HTTP (getImages, getImageById) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Feature.Images.PG (allImages, searchById, searchByTags)
import Feature.Images.Types (Image (..), Metadata (..), Response (..))
import Feature.Tags.PG (tagsByImgId)
import Feature.Tags.Types (label)
import Network.HTTP.Types (status200)
import Web.Scotty

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
