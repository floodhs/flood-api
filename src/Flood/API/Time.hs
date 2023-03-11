module Flood.API.Time
  ( UTCFloodTime(..)
  , NominalDiffFloodTime(..)
  )
  where

import Data.Aeson
import Data.Data
import Data.Coerce
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics

newtype UTCFloodTime = UTCFloodTime UTCTime
  deriving stock (Generic, Data)
  deriving newtype Show

instance ToJSON UTCFloodTime where
  toJSON = Number . realToFrac . utcTimeToPOSIXSeconds . coerce

instance FromJSON UTCFloodTime where
  parseJSON = withScientific "UTCFloodTime" (pure . UTCFloodTime . posixSecondsToUTCTime . realToFrac)

newtype NominalDiffFloodTime = NominalDiffFloodTime NominalDiffTime
  deriving stock (Generic, Data)
  deriving newtype Show

instance ToJSON NominalDiffFloodTime where
  toJSON = Number . realToFrac @NominalDiffTime . coerce

instance FromJSON NominalDiffFloodTime where
  parseJSON = withScientific "UTCFloodTime" (pure . NominalDiffFloodTime . realToFrac)
