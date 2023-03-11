module Flood.API.Auth
  ( authenticate
  , verify
  )
  where

import Control.Monad.Reader
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IntMap qualified as IntMap
import Flood.API.Effect
import Network.HTTP.Req

authenticate :: FloodT IO ()
authenticate = do
  config <- asks (.config)
  let body = KeyMap.fromList [("username", config.username), ("password", config.password)]
  callJSON POST ["auth", "authenticate"] mempty (ReqBodyJson body) $ IntMap.fromList
    [ (401, "incorrect username or password")
    , (422, "request validation error")
    ]

verify :: FloodT IO ()
verify =
  callJSON GET ["auth", "verify"] mempty NoReqBody $ IntMap.fromList
    [ (401, "not authenticated or token expired")
    , (500, "authentication succeeded but user is unattached")
    ]
