{-# LANGUAGE OverloadedStrings #-}

module Feature.Images.HTTP (getImages, getImageById, uploadImage) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Either (rights)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Feature.Images.PG (addImage, allImages, searchById, searchByTags)
import Feature.Images.Requests (rekognize, uploadImageToS3)
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
    parseParam,
    post,
    rescue,
    status,
    text,
  )

getMeta :: [Image] -> IO [Metadata]
getMeta imgs = forM imgs $ \img -> do
  tags <- tagsByImgId $ imageId img
  return Metadata {metadata = img, tags = map label tags}

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

addTagIfNotExists :: T.Text -> IO Int
addTagIfNotExists t = do
  ts <- tagByLabel t
  case ts of
    [] -> addTag t >>= return . tagId
    ts -> return . tagId . head $ ts

procTagParams :: T.Text -> IO [Int]
procTagParams "" = return []
procTagParams tags = do
  let ts = toList tags
  forM ts addTagIfNotExists

procTagPairs :: Int -> [Int] -> IO ()
procTagPairs imgId tagIds = forM_ tagIds $ \tagId -> addTagPair imgId tagId

procAutoTag :: Bool -> T.Text -> IO [Int]
procAutoTag False _ = return []
procAutoTag True fname = do
  ts <- rekognize fname
  forM ts addTagIfNotExists

toList :: T.Text -> [T.Text]
toList s = T.splitOn (T.pack ",") $ T.filter (/= '\"') s

toBool :: T.Text -> Bool
toBool "true" = True
toBool _ = False

uploadImage :: ScottyM ()
uploadImage = post "/images" $ do
  fs <- files
  let ftype = TL.pack . show . fileContentType . snd . head $ fs
  let fname = TL.toStrict . fst . head $ fs
  let fcontent = B.toStrict . fileContent . snd . head $ fs
  tagParams <- param "objects" `rescue` (\_ -> return ("" :: T.Text))
  aTagParam <- param "autotag" `rescue` (\_ -> return ("false" :: T.Text))
  img <- liftIO $ do
    uri <- uploadImageToS3 fname fcontent
    img <- addImage uri $ TL.toStrict ftype
    tagIds <- procTagParams tagParams
    aTagIds <- procAutoTag (toBool aTagParam) fname
    procTagPairs (imageId img) (tagIds ++ aTagIds)
    tags <- tagsByImgId (imageId img)
    return Metadata {metadata = img, tags = map label tags}
  status status200
  json $ Response {items = [img]}
