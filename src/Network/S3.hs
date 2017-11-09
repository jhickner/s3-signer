{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.S3
    ( -- * Create pre-signed AWS S3 URL
      generateS3URL
      -- * Types
    , module Network.S3.Types
    ) where

import           Data.Time
import           Network.S3.Sign
import           Network.S3.Time
import           Network.S3.Types
import           Network.S3.URL

-- | Generates a cryptographically secure URL that expires within a
-- user defined period
--
-- See README for use cases and examples: <https://github.com/dmjio/s3-signer/blob/master/README.md>
generateS3URL :: S3Keys -- ^ Amazon S3 Keys
              -> S3Request  -- ^ Amazon S3 Request information
              -> UTCTime -- ^ Expiration timestamp
              -> S3URL -- ^ Generated URL
generateS3URL S3Keys{..} S3Request{..} expr = S3URL req
  where
    ts = getExpirationTimeStamp expr
    url = case s3method of
            S3GET -> getURL bucketName objectName ts
            S3PUT -> putURL bucketName objectName ts mimeType
    req = s3URL bucketName objectName publicKey ts (sign secretKey url)
