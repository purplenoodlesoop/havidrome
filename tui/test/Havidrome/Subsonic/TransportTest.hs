{- | What a request that left the machine comes back as: a body, or the
failure the status or the exception says it is.
-}
module Havidrome.Subsonic.TransportTest (tests) where

import Data.ByteString (ByteString)
import Havidrome.Check (example)
import Havidrome.Subsonic (SubsonicError (AuthRejected, NetworkFailure, ServerFailure))
import Havidrome.Subsonic.Transport (classifyException, classifyStatus)
import Hedgehog (Gen, Group (Group), assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client
  ( HttpException (HttpExceptionRequest)
  , HttpExceptionContent (ConnectionTimeout, ResponseTimeout)
  , parseRequest_
  )
import Network.HTTP.Types.Status (mkStatus, status200, status401, status403, status500)

tests :: Group
tests =
  Group
    "Havidrome.Subsonic.Transport"
    [
      ( "reads a body the server answered with a 2xx"
      , example (classifyStatus status200 "body" === Right "body")
      )
    ,
      ( "reads a 401 and a 403 as a refusal of these credentials"
      , example do
          assert (all (refusal . (`classifyStatus` "")) [status401, status403])
      )
    ,
      ( "reads any other status as the server failing"
      , example do
          assert (case classifyStatus status500 "" of Left (ServerFailure 500 _) -> True; _ -> False)
      )
    ,
      ( "reads an unreachable host as a network failure"
      , example do
          assert . networkFailure $
            classifyException
              (HttpExceptionRequest (parseRequest_ "https://nowhere.invalid") ConnectionTimeout)
      )
    ,
      ( "hands back whatever body came with a 2xx, whichever 2xx it was"
      , property do
          code <- forAll (Gen.int (Range.linear 200 299))
          body <- forAll payload
          classifyStatus (mkStatus code "whatever") body === Right body
      )
    ,
      ( "reads a 401 or a 403 as a refusal whatever body came with it"
      , property do
          code <- forAll (Gen.element [401, 403])
          body <- forAll payload
          assert (refusal (classifyStatus (mkStatus code "Unauthorized") body))
      )
    ,
      ( "reads every other status as the server failing, under that very code"
      , property do
          code <- forAll (Gen.filter neitherSuccessNorRefusal (Gen.int everyCode))
          body <- forAll payload
          blamed (classifyStatus (mkStatus code "Trouble") body) === Just code
      )
    ,
      ( "reads anything http-client throws as the server never having been reached"
      , property do
          address <- forAll (Gen.element ["https://nowhere.invalid", "http://127.0.0.1:1/x"])
          content <- forAll (Gen.element [ConnectionTimeout, ResponseTimeout])
          assert . networkFailure $
            classifyException (HttpExceptionRequest (parseRequest_ address) content)
      )
    ]

-- | The bodies a server can answer with, an empty one among them.
payload :: Gen ByteString
payload = Gen.bytes (Range.linear 0 32)

-- | Every status code a reply can carry.
everyCode :: Range.Range Int
everyCode = Range.linear 100 599

-- | Whether a status is one the player reads as neither a body nor a refusal.
neitherSuccessNorRefusal :: Int -> Bool
neitherSuccessNorRefusal code = not (code >= 200 && code < 300) && code `notElem` [401, 403]

-- | Whether the credentials themselves were turned down.
refusal :: Either SubsonicError a -> Bool
refusal = \case
  Left (AuthRejected _) -> True
  _ -> False

{- | The status code a failure blames the server under, if that is what it
blames.
-}
blamed :: Either SubsonicError a -> Maybe Int
blamed = \case
  Left (ServerFailure code _) -> Just code
  _ -> Nothing

-- | Whether the server was never reached at all.
networkFailure :: SubsonicError -> Bool
networkFailure = \case
  NetworkFailure _ -> True
  _ -> False
