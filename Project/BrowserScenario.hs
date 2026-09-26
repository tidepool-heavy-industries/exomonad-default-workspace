{-# LANGUAGE OverloadedStrings #-}

-- | Select one existing deterministic production browser integration test.
-- The tests own authentication, loopback ports, temporary stores and WebSocket
-- behavior; this module supplies no browser transport.
module Project.BrowserScenario
  ( BrowserScenario (..)
  , browserFocusedSpec
  , browserPreparationCommand
  , browserReadinessCommand
  ) where

import Data.Text (Text)
import Project.TestEvidence (FocusedSpec (..))
import qualified Tidepool.Command as Cmd

data BrowserScenario
  = BrowserJourney
  | StandaloneReopen
  deriving (Show, Eq)

-- | The web prerequisites from `scripts/verify-browser-journey`, ending
-- before that script's browser assertion. The preparation owner runs this in
-- the selected checkout with an explicit memory budget and retains its job.
browserPreparationCommand :: Cmd.Command
browserPreparationCommand = Cmd.argv
  [ "nix", "develop", ".#web", "-c", "bash", "-lc"
  , "cd web && npm ci && npm run check && npm test && npm run build"
  ]

-- | A second read in the same checkout after preparation. Its completed
-- result is evidence for the prepared asset required by the production tests.
browserReadinessCommand :: Cmd.Command
browserReadinessCommand = Cmd.argv ["test", "-f", "web/dist/index.html"]

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
