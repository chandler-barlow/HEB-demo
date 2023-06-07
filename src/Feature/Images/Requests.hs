{-# LANGUAGE OverloadedStrings #-}

module Feature.Images.Requests (uploadImageToS3, rekognize) where

import qualified Amazonka as AWS
import Amazonka.Auth (Env' (..))
import qualified Amazonka.Rekognition as RK
import qualified Amazonka.Rekognition.DetectLabels as DL
import qualified Amazonka.Rekognition.Types as RT
import qualified Amazonka.S3 as S3
import Amazonka.S3.PutObject (newPutObject)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Feature.Images.Types (Image (..))
import Util.Config (AppConfig (..), appConfig)

uploadImageToS3 :: T.Text -> B.ByteString -> IO T.Text
uploadImageToS3 fileName fileContent = do
  env <- AWS.newEnv AWS.discover
  conf <- appConfig
  let s3Req = newPutObject bucket objKey body
        where
          bucket = S3.BucketName $ s3BucketName conf
          objKey = S3.ObjectKey fileName
          body = AWS.toBody fileContent
  AWS.runResourceT $ AWS.send env s3Req
  return $ "https://" <> (s3BucketName conf) <> ".s3.amazonaws.com/" <> fileName

justs :: [Maybe a] -> [a]
justs = foldr f []
  where
    f (Just x) xs = x : xs
    f Nothing xs = xs

-- using the s3 link in Image to get rekognition labels
rekognize :: T.Text -> IO [T.Text]
rekognize fname =
  do
    conf <- appConfig
    env <- AWS.newEnv AWS.discover
    let s3Obj =
          RT.newS3Object
            & RT.s3Object_bucket ?~ (s3BucketName conf)
            & RT.s3Object_name ?~ fname
    let img = RT.newImage & RT.image_s3Object ?~ s3Obj
    let imgReq =
          DL.newDetectLabels img
            & DL.detectLabels_maxLabels ?~ 10
    res <- (AWS.runResourceT $ AWS.send env imgReq) :: IO DL.DetectLabelsResponse
    let labels = case res ^. DL.detectLabelsResponse_labels of
          Nothing -> []
          Just x -> x
    let labelNames = justs $ map (view RT.label_name) labels
    return labelNames
