module Flood.API.Client
  -- * Connection
  ( Connection(..), connectionTest
  -- * Settings
  , Settings(..), getSettings, updateSettings
  )
  where

import Data.Aeson
import Data.IntMap qualified as IntMap
import Data.Text (Text)
import Data.Word
import Flood.API.Effect
import GHC.Generics
import GHC.Natural
import Network.HTTP.Req

newtype Connection = Connection
  { isConnected :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

connectionTest :: FloodT IO Connection
connectionTest =
  callJSON GET ["client", "connection-test"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]


data Settings = Settings
  { dht :: Bool
  , dhtPort :: Word16
  , directoryDefault :: Text
  , networkHttpMaxOpen :: Natural
  , networkLocalAddress :: [Text]
  , networkMaxOpenFiles :: Natural
  , networkPortOpen :: Bool
  , networkPortRandom :: Bool
  , networkPortRange :: Text
  , piecesHashOnCompletion :: Bool
  , piecesMemoryMax :: Natural
  , protocolPex :: Bool
  , throttleGlobalDownSpeed :: Natural
  , throttleGlobalUpSpeed :: Natural
  , throttleMaxPeersNormal :: Natural
  , throttleMaxPeersSeed :: Natural
  , throttleMaxDownloads :: Natural
  , throttleMaxDownloadsGlobal :: Natural
  , throttleMaxUploads :: Natural
  , throttleMaxUploadsGlobal :: Natural
  , throttleMinPeersNormal :: Natural
  , throttleMinPeersSeed :: Natural
  , trackersNumWant :: Natural
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

getSettings :: FloodT IO Settings
getSettings =
  callJSON GET ["client", "settings"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]

-- |Flood seems to return a 500 error message on success?
updateSettings :: Settings -> FloodT IO ()
updateSettings settings =
  callJSON PATCH ["client", "settings"] mempty (ReqBodyJson settings) $ IntMap.empty
