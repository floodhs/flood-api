module Flood.API.Frontend
  -- * Transfer history
  ( SnapshotLength(..), History(..), HistoryEntry(..), history
  -- * Notifications
  , NotificationData(..), Notification(..), NotificationCount(..), NotificationList(..)
  , getNotifications, deleteNotifications
  -- * Settings
  , Direction(..), Column(..), Action(..), Visibility(..), ViewSize(..), TagSelectorMode(..)
  , TorrentsAddTab(..), SpeedLimits(..), SortSettings(..), Settings(..), getSettings, updateSettings
  )
  where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.IntMap qualified as IntMap
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Flood.API.Effect
import Flood.API.Internal.Aeson
import Flood.API.Time
import GHC.Generics
import GHC.Natural
import Network.HTTP.Req
import Optics

data SnapshotLength
  = FiveMinute
  | ThirtyMinute
  | Hour
  | Day
  | Week
  | Month
  | Year
  deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = shoutString } ''SnapshotLength)

data History = History
  { upload, download :: Vector (Maybe Double)
  , timestamps :: Vector Natural
  } deriving (Generic, Show, ToJSON, FromJSON)

data HistoryEntry = HistoryEntry
  { upload, download :: Maybe Double
  , timestamp :: Natural
  } deriving (Generic, Show, ToJSON, FromJSON)

historyEntries :: History -> [HistoryEntry]
historyEntries history =
  getZipList $
    liftA3
      (\upload download timestamp -> HistoryEntry { .. })
      (ZipList (Vector.toList history.upload))
      (ZipList (Vector.toList history.download))
      (ZipList (Vector.toList history.timestamps))

history :: SnapshotLength -> FloodT IO [HistoryEntry]
history length =
  fmap historyEntries $
    callJSON GET ["history"] ("snapshot" =: pack (shoutString (show length))) NoReqBody $ IntMap.fromList
      [ (500, "failure response")
      ]


newtype NotificationData = NotificationData
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data Notification = Notification
  { ts :: UTCFloodTime
  , data_ :: NotificationData
  , id :: Text
  , read :: Bool
  , _id :: Text
  } deriving (Generic, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = _tail %~ filter (/= '_') } ''Notification)

data NotificationCount = NotificationCount
  { read, total, unread :: Natural
  } deriving (Generic, Show, ToJSON, FromJSON)

data NotificationList = NotificationList
  { notifications :: [Notification]
  , count :: NotificationCount
  } deriving (Generic, Show, ToJSON, FromJSON)

getNotifications :: FloodT IO NotificationList
getNotifications =
  callJSON GET ["notifications"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]

deleteNotifications :: FloodT IO ()
deleteNotifications =
  callJSON DELETE ["notifications"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]


data Direction
  = Ascending
  | Descending
  deriving (Generic, Show)

instance ToJSON Direction where
  toJSON = \case
    Ascending -> "asc"
    Descending -> "desc"

instance FromJSON Direction where
  parseJSON = withText "Direction" \case
    "asc" -> pure Ascending
    "desc" -> pure Descending
    unexpected -> fail ("unexpected Direction: " ++ unpack unexpected)

data Column
  = DateAdded
  | DateFinished
  | DownRate
  | DownTotal
  | Eta
  | Name
  | Peers
  | PercentComplete
  | Ratio
  | Seeds
  | SizeBytes
  | Tags
  | UpRate
  | UpTotal
  | DateCreated
  | Directory
  | Hash
  | IsPrivate
  | Message
  | TrackerURIs
  deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = _head %~ toLower } ''Column)

data Action
  = Start
  | Stop
  | Remove
  | CheckHash
  | Reannounce
  | SetTaxonomy
  | Move
  | SetTrackers
  | TorrentDetails
  | DownloadContents
  | DownloadMetainfo
  | GenerateMagnet
  | SetInitialSeeding
  | SetSequential
  | SetPriority
  deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = _head %~ toLower } ''Action)

data Visibility a = Visibility
  { id :: a
  , visible :: Bool
  } deriving (Generic, Show, ToJSON, FromJSON)

data ViewSize
  = Condensed
  | Expanded
  deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = _head %~ toLower } ''ViewSize)

data TagSelectorMode
  = Single
  | Multi
  deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = _head %~ toLower } ''TagSelectorMode)

data TorrentsAddTab
  = ByUrl
  | ByFile
  | ByCreation
  deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = kebabString } ''TorrentsAddTab)

data SpeedLimits = SpeedLimits
  { download, upload :: [Natural]
  } deriving (Generic, Show, ToJSON, FromJSON)

data SortSettings = SortSettings
  { direction :: Direction
  , property :: Column
  } deriving (Generic, Show, ToJSON, FromJSON)

data Settings = Settings
  { language :: Text
  , sortTorrents :: Maybe SortSettings
  , torrentListColumns :: Maybe [Visibility Column]
  , torrentListColumnWidths :: Maybe (HashMap String Natural)
  , torrentContextMenuActions :: Maybe [Visibility Action]
  , torrentListViewSize :: Maybe ViewSize
  , speedLimits :: Maybe SpeedLimits
  , mountPoints :: Maybe [Text]
  , deleteTorrentData :: Maybe Bool
  , startTorrentsOnLoad :: Maybe Bool
  , torrentDestinations :: Maybe (HashMap Text Text)
  , uiTagSelectorMode :: Maybe TagSelectorMode
  , uiTorrentsAddTab :: Maybe TorrentsAddTab
  } deriving (Generic, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = unpack . Text.replace "ui" "UI" . pack } ''Settings)

getSettings :: FloodT IO Settings
getSettings =
  callJSON GET ["settings"] mempty NoReqBody $ IntMap.fromList
    [ (500, "failure response")
    ]

updateSettings :: Settings -> FloodT IO ()
updateSettings settings =
  callJSON PATCH ["settings"] mempty (ReqBodyJson settings) $ IntMap.fromList
    [ (500, "failure response")
    ]
