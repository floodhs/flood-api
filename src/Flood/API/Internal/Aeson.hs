module Flood.API.Internal.Aeson where

import Data.Char
import Data.List qualified as List

shoutString :: String -> String
shoutString = fmap toUpper . concat . List.intersperse "_" . List.groupBy (\_ y -> not (isUpper y))

kebabString :: String -> String
kebabString = fmap toLower . concat . List.intersperse "-" . List.groupBy (\_ y -> not (isUpper y))
