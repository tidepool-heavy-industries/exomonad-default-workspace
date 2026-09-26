{-# LANGUAGE OverloadedStrings #-}

-- | Select one existing deterministic production browser integration test.
-- The tests own authentication, loopback ports, temporary stores and WebSocket
-- behavior; this module supplies no browser transport.
module Project.BrowserScenario
  ( BrowserScenario (..)
  , browserFocusedSpec
  ) where

import Data.Text (Text)
import Project.TestEvidence (FocusedSpec (..))

data BrowserScenario
  = BrowserJourney
  | StandaloneReopen
  deriving (Show, Eq)

browserFocusedSpec :: Text -> BrowserScenario -> FocusedSpec
browserFocusedSpec source scenario = case scenario of
  BrowserJourney -> FocusedSpec
    { focusedIntent = "authenticated browser command, pending/cancel, child failure and reconnect"
    , focusedSource = source
    , focusedPackage = "harness-demo"
    , focusedTarget = "test:browser_journey"
    , focusedFilter = "browser_journey_auth_pending_cancel_child_failure_and_reconnect"
    , focusedExpected = 1
    }
  StandaloneReopen -> FocusedSpec
    { focusedIntent = "standalone browser asset, clean restart and process-loss reopen"
    , focusedSource = source
    , focusedPackage = "harness-demo"
    , focusedTarget = "test:standalone_browser"
    , focusedFilter = "standalone_missing_assets_and_clean_and_process_loss_reopen"
    , focusedExpected = 1
    }
