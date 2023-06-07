module Main (main) where

import Feature.Images.HTTP (getImageById, getImages, uploadImage)
import Util.Config (AppConfig (..), appConfig)
import Web.Scotty (scotty)

main :: IO ()
main = do
  config <- appConfig
  scotty (port config) $ do
    getImages
    getImageById
    uploadImage
