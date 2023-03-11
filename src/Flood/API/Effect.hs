module Flood.API.Effect
  -- * Runtime state
  -- ** Errors
  ( FloodError(..)
  -- ** Configuration
  , FloodConfig(..)
  -- ** Connection
  , FloodInstance(..), newFloodInstance
  -- * The `FloodT` transformer
  , FloodT, runFloodT
  -- * Calling the Flood API
  , callJSON, callBS, call
  )
  where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Optics
import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Proxy
import Data.Some
import Data.Text (Text)
import GHC.Generics
import GHC.TypeNats
import Network.HTTP.Client (CookieJar, HttpExceptionContent(..), HttpException(..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req as Req
import Network.HTTP.Types qualified as HTTP
import Optics
import Text.URI
import UnliftIO.Exception
import UnliftIO.IORef

data FloodError (code :: Nat) = FloodError
  { description :: Maybe Text
  , message :: Maybe Text
  } deriving (Exception, Generic)

instance KnownNat code => Show (FloodError code) where
  show FloodError { description, message } =
    "FloodError { description = " ++ show description ++
    ", message = " ++ show message ++
    " } :: FloodError " ++ show (natVal (Proxy @code))

data FloodConfig = FloodConfig
  { url :: Text
  , port :: Int
  , username :: Text
  , password :: Text
  } deriving (Generic, Show)

data FloodInstance = FloodInstance
  { config :: FloodConfig
  , cookies :: IORef CookieJar
  } deriving Generic

newFloodInstance :: MonadIO m => FloodConfig -> m FloodInstance
newFloodInstance config = do
  cookies <- newIORef mempty
  pure FloodInstance { .. }

type FloodT = ReaderT FloodInstance

runFloodT :: MonadIO m => FloodConfig -> FloodT m a -> m a
runFloodT cfg m = runReaderT m =<< newFloodInstance cfg

data InvalidHostURL = InvalidHostURL Text
  deriving (Generic, Show, Exception)

callJSON ::
  forall response method body.
  ( HttpMethod method, HttpBody body
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
  , FromJSON response
  ) =>
  method -> [Text] -> (forall scheme. Option scheme) -> body -> IntMap Text -> FloodT IO response
callJSON method functionParts callOption body =
  fmap (responseBody @(JsonResponse response)) . call method functionParts callOption body

callBS ::
  forall method body.
  ( HttpMethod method, HttpBody body
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
  ) =>
  method -> [Text] -> (forall scheme. Option scheme) -> body -> IntMap Text -> FloodT IO ByteString
callBS method functionParts callOption body =
  fmap (responseBody @BsResponse) . call method functionParts callOption body

call ::
  forall method body response.
  ( HttpMethod method, HttpBody body, HttpResponse response
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
  ) =>
  method -> [Text] -> (forall scheme. Option scheme) -> body -> IntMap Text -> FloodT IO response
call method functionParts callOption body describe = do
  flood <- ask
  cookies <- readIORef flood.cookies
  let
    checkResponse request response preview =
      if HTTP.statusCode (HTTP.responseStatus response) == 207 then
        Just $ StatusCodeException (void response) preview
      else
        httpConfigCheckResponse defaultHttpConfig request response preview
  Some url <- case useURI =<< mkURI flood.config.url of
    Nothing -> throwIO $ InvalidHostURL flood.config.url
    Just (Left (httpUrl, _)) -> pure $ Some httpUrl
    Just (Right (httpsUrl, _)) -> pure $ Some httpsUrl
  let
    request = req method (foldl (/~) (url /: "api") functionParts) body Proxy option
    option = callOption <> port flood.config.port <> cookieJar cookies
  catch
    do
      response <- runReq defaultHttpConfig { httpConfigCheckResponse = checkResponse } request
      modifyIORef flood.cookies (<> responseCookieJar response)
      pure response
    do
      \(e :: Req.HttpException) ->
        case e of
          VanillaHttpException (HttpExceptionRequest _ (StatusCodeException response body)) ->
            let code = responseStatusCode response in
              case someNatVal (fromIntegral code) of
                SomeNat (_ :: Proxy code) ->
                  throwIO @_ @(FloodError code) FloodError
                    { description = describe ^? ix code
                    , message = body ^? key "message" % _String
                    }
          _ -> throwIO e
