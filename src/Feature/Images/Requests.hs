{-# LANGUAGE OverloadedStrings #-}

module Feature.Images.Requests (uploadImageToS3) where

import qualified Amazonka as AWS
import Amazonka.Auth (Env' (..))
import qualified Amazonka.Rekognition as RK
import qualified Amazonka.S3 as S3
import Amazonka.S3.PutObject (newPutObject)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
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
