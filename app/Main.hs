module Main (main) where

import Feature.Images.HTTP (getImageById, getImages)
import Web.Scotty (scotty)

main :: IO ()
main = do
  scotty 3000 $ do
    getImages
    getImageById
