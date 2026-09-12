-- | What a request that left the machine comes back as: a body, or the
-- failure the status or the exception says it is.
module Havidrome.Subsonic.TransportSpec (spec) where

import Havidrome.Subsonic (SubsonicError (AuthRejected, NetworkFailure, ServerFailure))
import Havidrome.Subsonic.Transport (classifyException, classifyStatus)
import Network.HTTP.Client
  ( HttpException (..)
  , HttpExceptionContent (..)
  , parseRequest_
  )
import Network.HTTP.Types.Status (status200, status401, status403, status500)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "classifying failures" $ do
  it "reads a body the server answered with a 2xx" $
    classifyStatus status200 "body" `shouldBe` Right "body"

  it "reads a 401 and a 403 as a refusal of these credentials" $
    map (fmap (const ()) . (`classifyStatus` "")) [status401, status403]
      `shouldSatisfy` all (\answer -> case answer of Left (AuthRejected _) -> True; _ -> False)

  it "reads any other status as the server failing" $
    classifyStatus status500 "" `shouldSatisfy` \answer -> case answer of
      Left (ServerFailure 500 _) -> True
      _ -> False

  it "reads an unreachable host as a network failure" $
    classifyException
      (HttpExceptionRequest (parseRequest_ "https://nowhere.invalid") ConnectionTimeout)
      `shouldSatisfy` \failure -> case failure of
        NetworkFailure _ -> True
        _ -> False
