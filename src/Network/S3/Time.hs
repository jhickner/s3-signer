module Network.S3.Time
    ( getExpirationTimeStamp
    ) where

import           Data.ByteString.UTF8  (ByteString, fromString)
import           Data.Time             (UTCTime (..))
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

getExpirationTimeStamp :: UTCTime -> ByteString
getExpirationTimeStamp ts =
  fromString . show $ epochTime
    where
      epochTime :: Integer
      epochTime = round . utcTimeToPOSIXSeconds $ ts
