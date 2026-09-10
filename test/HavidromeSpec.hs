{-# LANGUAGE OverloadedStrings #-}

-- | The player as a whole, at the points where it has nowhere to browse.
module HavidromeSpec (spec) where

import Control.Exception (bracket, finally)
import Data.Text (Text)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Havidrome (run)
import Havidrome.Credentials (Credentials (Credentials), save)
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure))
import System.IO (IOMode (WriteMode), hClose, stderr, withFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldThrow)

spec :: Spec
spec = describe "run" $ do
  it "stops instead of browsing when no credentials are stored" $
    withConfigHome $
      quietly run `shouldThrow` (== ExitFailure 1)

  it "stops instead of browsing when the artist list cannot be fetched" $
    withConfigHome $ do
      save (Credentials nowhere "someone" "secret")
      quietly run `shouldThrow` (== ExitFailure 1)

-- | An address nothing answers on, so that the artist list fails to arrive
-- the way it fails against a server that cannot be reached.
nowhere :: Text
nowhere = "http://127.0.0.1:1"

-- | An empty config directory of its own, so that the specs never read the
-- credentials of whoever is running them.
withConfigHome :: IO a -> IO a
withConfigHome action =
  withSystemTempDirectory "havidrome-config" $ \home ->
    bracket (setEnv "XDG_CONFIG_HOME" home) (const (unsetEnv "XDG_CONFIG_HOME")) (const action)

-- | Runs something with its complaints sent nowhere, so that a spec about
-- stopping does not print the reason among the results.
quietly :: IO a -> IO a
quietly action =
  withFile "/dev/null" WriteMode $ \sink ->
    bracket (hDuplicate stderr) hClose $ \saved -> do
      hDuplicateTo sink stderr
      action `finally` hDuplicateTo saved stderr
