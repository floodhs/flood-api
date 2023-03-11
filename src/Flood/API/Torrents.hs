module Flood.API.Torrents
  ( Hash(..), Tag(..), TrackerURI(..)
  -- * Status
  , Status(..), Info(..), List(..), list
  -- * Add by URLs
  , AddUrlsSettings(..), defaultAddUrlsSettings, addUrls
  -- * Add by files
  , AddFilesSettings(..), defaultAddFilesSettings, addFiles
  -- * Control torrent state
  , start, stop, hashCheck, reannounce
  -- * Move
  , MoveSettings(..), defaultMoveSettings, move
  -- * Delete
  , DeleteSettings(..), delete
  -- * Update settings
  -- ** Initial seeding
  , InitialSeedingSettings(..), updateInitialSeeding
  -- ** Priority
  , Priority(..), PrioritySettings(..), updatePriority
  -- ** Sequential
  , SequentialSettings(..), updateSequential
  -- ** Tags
  , TagsSettings(..), updateTags
  -- ** Trackers
  , TrackersSettings(..), updateTrackers
  -- * Metainfo
  , metainfo
  -- * Contents
  , TorrentContents(..), contents
  -- * Download tokens
  , token
  -- * Details
  , Content(..), Tracker(..), Peer(..), Details(..), details, peers, trackers
  -- * Mediainfo
  , MediaInfo(..), mediainfo
  )
  where

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Char
import Data.Coerce
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Map (Map)
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.String
import Data.Text (Text)
import Data.Text.Encoding
import Flood.API.Effect
import Flood.API.Time
import GHC.Generics
import GHC.Natural
import Network.HTTP.Req

newtype Hash = Hash { text :: Text }
  deriving stock Generic
  deriving newtype (Show, IsString, ToJSON, FromJSON)

newtype Tag = Tag { text :: Text }
  deriving stock Generic
  deriving newtype (Show, IsString, ToJSON, FromJSON)

newtype TrackerURI = TrackerURI { text :: Text }
  deriving stock Generic
  deriving newtype (Show, IsString, ToJSON, FromJSON)

data Status
  = Complete
  | Seeding
  | Active
  | Inactive
  | Stopped
  | Checking
  | Error
  deriving (Generic, Show, Eq, Read)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower } ''Status)

data Info = Info
  { bytesDone :: Natural
  , dateActive :: Maybe UTCFloodTime
  , dateAdded :: Maybe UTCFloodTime
  , dateCreated :: Maybe UTCFloodTime
  , dateFinished :: Maybe UTCFloodTime
  , directory :: Text
  , downRate :: Natural
  , downTotal :: Natural
  , eta :: Maybe NominalDiffFloodTime
  , hash :: Hash
  , isPrivate :: Bool
  , isInitialSeeding :: Bool
  , isSequential :: Bool
  , message :: Text
  , name :: Text
  , peersConnected
  , peersTotal :: Natural
  , percentComplete :: Double
  , priority :: Natural
  , ratio :: Double
  , seedsConnected
  , seedsTotal :: Natural
  , sizeBytes :: Natural
  , status :: [Status]
  , tags :: [Tag]
  , trackerURIs :: [TrackerURI]
  , upRate :: Natural
  , upTotal :: Natural
  } deriving (Generic, Show, ToJSON, FromJSON)

data List = List
  { id :: Natural
  , torrents :: Map Text Info
  } deriving (Generic, Show, ToJSON, FromJSON)

list :: FloodT IO List
list =
  callJSON GET ["torrents"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]


data AddUrlsSettings = AddUrlsSettings
  { urls :: List.NonEmpty Text
  , cookies :: Maybe (List.NonEmpty Text)
  , destination :: Maybe Text
  , tags :: Maybe (List.NonEmpty Tag)
  , isBasePath :: Maybe Bool
  , isCompleted :: Maybe Bool
  , isSequential :: Maybe Bool
  , isInitialSeeding :: Maybe Bool
  , start :: Maybe Bool
  } deriving (Generic, Show)

$(deriveJSON defaultOptions { omitNothingFields = True } ''AddUrlsSettings)

defaultAddUrlsSettings :: List.NonEmpty Text -> AddUrlsSettings
defaultAddUrlsSettings urls = AddUrlsSettings
  { urls
  , cookies = Nothing
  , destination = Nothing
  , tags = Nothing
  , isBasePath = Nothing
  , isCompleted = Nothing
  , isSequential = Nothing
  , isInitialSeeding = Nothing
  , start = Nothing
  }

addUrls :: AddUrlsSettings -> FloodT IO ()
addUrls settings = do
  callJSON POST ["torrents", "add-urls"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (207, "some succeeded and some failed")
    , (403, "not allowed")
    , (422, "malformed input")
    , (500, "failure response")
    ]


data AddFilesSettings = AddFilesSettings
  { files :: List.NonEmpty Text
  , destination :: Maybe Text
  , tags :: Maybe (List.NonEmpty Tag)
  , isBasePath :: Maybe Bool
  , isCompleted :: Maybe Bool
  , isSequential :: Maybe Bool
  , isInitialSeeding :: Maybe Bool
  , start :: Maybe Bool
  } deriving (Generic, Show)

$(deriveJSON defaultOptions { omitNothingFields = True } ''AddFilesSettings)

defaultAddFilesSettings ::
  List.NonEmpty Text -> -- ^ base64-encoded torrent files
  AddFilesSettings
defaultAddFilesSettings files = AddFilesSettings
  { files
  , destination = Nothing
  , tags = Nothing
  , isBasePath = Nothing
  , isCompleted = Nothing
  , isSequential = Nothing
  , isInitialSeeding = Nothing
  , start = Nothing
  }

addFiles :: AddFilesSettings -> FloodT IO ()
addFiles settings = do
  callJSON POST ["torrents", "add-files"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (207, "some succeeded and some failed")
    , (403, "not allowed")
    , (422, "malformed input")
    , (500, "failure response")
    ]


newtype Hashes = Hashes
  { hashes :: [Hash]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

start :: [Hash] -> FloodT IO ()
start hashes =
  callJSON POST ["torrents", "start"] mempty (ReqBodyJson Hashes { hashes }) $ IntMap.fromList
    [ (500, "failure response")
    ]

stop :: [Hash] -> FloodT IO ()
stop hashes =
  callJSON POST ["torrents", "stop"] mempty (ReqBodyJson Hashes { hashes }) $ IntMap.fromList
    [ (500, "failure response")
    ]

hashCheck :: [Hash] -> FloodT IO ()
hashCheck hashes =
  callJSON POST ["torrents", "check-hash"] mempty (ReqBodyJson Hashes { hashes }) $ IntMap.fromList
    [ (500, "failure response")
    ]

reannounce :: [Hash] -> FloodT IO ()
reannounce hashes =
  callJSON POST ["torrents", "reannounce"] mempty (ReqBodyJson Hashes { hashes }) $ IntMap.fromList
    [ (500, "failure response")
    ]


data MoveSettings = MoveSettings
  { hashes :: [Hash]
  , destination :: Text
  , moveFiles :: Maybe Bool
  , isBasePath :: Maybe Bool
  , isCheckHash :: Maybe Bool
  } deriving (Generic, Show, ToJSON, FromJSON)

defaultMoveSettings :: [Hash] -> Text -> MoveSettings
defaultMoveSettings hashes destination = MoveSettings
  { hashes, destination
  , moveFiles = Nothing
  , isBasePath = Nothing
  , isCheckHash = Nothing
  }

move :: MoveSettings -> FloodT IO ()
move settings =
  callJSON POST ["torrents", "move"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (403, "access denied")
    , (500, "failure response")
    ]


data DeleteSettings = DeleteSettings
  { hashes :: [Hash]
  , deleteData :: Bool
  } deriving (Generic, Show, ToJSON, FromJSON)

delete :: DeleteSettings -> FloodT IO ()
delete settings =
  callJSON POST ["torrents", "delete"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (500, "failure response")
    ]


data InitialSeedingSettings = InitialSeedingSettings
  { hashes :: [Hash]
  , isInitialSeeding :: Bool
  } deriving (Generic, Show, ToJSON, FromJSON)

updateInitialSeeding :: InitialSeedingSettings -> FloodT IO ()
updateInitialSeeding settings =
  callJSON PATCH ["torrents", "initial-seeding"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (500, "failure response")
    ]


data Priority = Priority0 | Priority1 | Priority2 | Priority3
  deriving (Generic, Show, Enum)

instance ToJSON Priority where
  toJSON = Number . fromIntegral . fromEnum

instance FromJSON Priority where
  parseJSON = withScientific "Priority" (pure . toEnum . truncate)

data PrioritySettings = PrioritySettings
  { hashes :: [Hash]
  , priority :: Priority
  } deriving (Generic, Show, ToJSON, FromJSON)

updatePriority :: PrioritySettings -> FloodT IO ()
updatePriority settings =
  callJSON PATCH ["torrents", "priority-seeding"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (500, "failure response")
    ]


data SequentialSettings = SequentialSettings
  { hashes :: [Hash]
  , isSequential :: Bool
  } deriving (Generic, Show, ToJSON, FromJSON)

updateSequential :: SequentialSettings -> FloodT IO ()
updateSequential settings =
  callJSON PATCH ["torrents", "sequential-seeding"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (500, "failure response")
    ]


data TagsSettings = TagsSettings
  { hashes :: [Hash]
  , tags :: [Tag]
  } deriving (Generic, Show, ToJSON, FromJSON)

updateTags :: TagsSettings -> FloodT IO ()
updateTags settings =
  callJSON PATCH ["torrents", "tags"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (500, "failure response")
    ]


data TrackersSettings = TrackersSettings
  { hashes :: [Hash]
  , trackers :: [TrackerURI]
  } deriving (Generic, Show, ToJSON, FromJSON)

updateTrackers :: TrackersSettings -> FloodT IO ()
updateTrackers settings =
  callJSON PATCH ["torrents", "tags"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (500, "failure response")
    ]


metainfo :: [Hash] -> FloodT IO ByteString
metainfo hashes =
  callBS GET ["torrents", mconcat $ List.intersperse "," (coerce hashes), "metainfo"] mempty NoReqBody $ IntMap.fromList
    [ (422, "hash not provided")
    , (500, "failure response")
    ]


data TorrentContents = TorrentContents
  { index :: Natural
  , path :: Text
  , filename :: Text
  , percentComplete :: Double
  , priority :: Priority
  , sizeBytes :: Natural
  } deriving (Generic, Show, ToJSON, FromJSON)

contents :: Hash -> FloodT IO [TorrentContents]
contents hash =
  callJSON GET ["torrents", coerce hash, "contents"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]


token :: Hash -> [Text] -> FloodT IO Text
token hash indices =
  fmap decodeUtf8 $
    callBS GET ["torrents", coerce hash, "contents", mconcat $ List.intersperse "," indices, "token"] mempty NoReqBody $ IntMap.fromList
      [ (500, "failure response")
      ]


data Content = Content
  { index :: Natural
  , path :: Text
  , filename :: Text
  , percentComplete :: Double
  , priority :: Priority
  , sizeBytes :: Natural
  } deriving (Generic, Show, ToJSON, FromJSON)

data Tracker = Tracker
  { url :: TrackerURI
  , type_ :: Natural
  } deriving (Generic, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = filter (/= '_') } ''Tracker)

data Peer = Peer
  { address :: Text
  , clientVersion :: Text
  , completedPercent :: Double
  , downloadRate :: Natural
  , uploadRate :: Natural
  , isEncrypted :: Bool
  , isIncoming :: Bool
  , country :: Text
  } deriving (Generic, Show, ToJSON, FromJSON)

data Details = Details
  { contents :: [Content]
  , peers :: [Peer]
  , trackers :: [Tracker]
  } deriving (Generic, Show, ToJSON, FromJSON)

details :: Hash -> FloodT IO Details
details hash =
  callJSON GET ["torrents", coerce hash, "details"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]


newtype MediaInfo = MediaInfo
  { output :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

mediainfo :: Hash -> FloodT IO Text
mediainfo hash =
  fmap coerce $
    callJSON @MediaInfo GET ["torrents", coerce hash, "mediainfo"] mempty NoReqBody $ IntMap.fromList
      [ (500, "failure response")
      ]


peers :: Hash -> FloodT IO [Peer]
peers hash =
  callJSON GET ["torrents", coerce hash, "peers"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]

trackers :: Hash -> FloodT IO [Tracker]
trackers hash =
  callJSON GET ["torrents", coerce hash, "trackers"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]
