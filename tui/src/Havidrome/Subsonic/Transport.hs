-- | How a request actually leaves the machine. The client is written against
-- 'Transport' rather than against @http-client@, so its behaviour can be
-- exercised without a server on the other end.
module Havidrome.Subsonic.Transport
  ( Transport (..)
  , httpTransport
  , newHttpTransport
  , classifyException
  , classifyStatus
  ) where

import Control.Exception (try)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)
import Havidrome.Subsonic.Types
import Network.HTTP.Client
  ( HttpException
  , Manager
  , httpLbs
  , parseRequest
  , responseBody
  , responseStatus
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)

-- | Fetches the body at a URL, or says why it could not. Every failure a GET
-- can suffer is already classified by the time it comes back.
newtype Transport = Transport
  { fetch :: Text -> IO (Either SubsonicError ByteString)
  }
  deriving stock (Generic)

-- | A transport that really speaks HTTP, over a manager the caller owns.
httpTransport :: Manager -> Transport
httpTransport manager = Transport $ \url ->
  case parseRequest (Text.unpack url) of
    Nothing ->
      pure (Left (NetworkFailure ("not a usable server address: " <> url)))
    Just request -> do
      attempt <- try (httpLbs request manager)
      pure $ case attempt of
        Left exception -> Left (classifyException exception)
        Right response ->
          classifyStatus
            (responseStatus response)
            (Lazy.toStrict (responseBody response))

-- | A transport with a TLS-capable manager of its own.
newHttpTransport :: IO Transport
newHttpTransport = httpTransport <$> newTlsManager

-- | Anything @http-client@ throws means the server was never reached: an
-- unknown host, a refused connection, a TLS failure, a timeout.
classifyException :: HttpException -> SubsonicError
classifyException exception =
  NetworkFailure ("could not reach the server: " <> Text.pack (show exception))

-- | A reply with a status. Subsonic reports its own failures inside a 200, so
-- anything else came from the server or something in front of it — and a 401
-- or 403 there is still a refusal of these credentials.
classifyStatus :: Status -> ByteString -> Either SubsonicError ByteString
classifyStatus status body
  | code >= 200 && code < 300 = Right body
  | code == 401 || code == 403 =
      Left (AuthRejected ("the server refused these credentials: " <> reason))
  | otherwise = Left (ServerFailure code reason)
 where
  code = statusCode status
  reason = Text.decodeUtf8Lenient (statusMessage status)
