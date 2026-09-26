{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Project.BrowserScenario
import Project.TestEvidence (FocusedSpec (..))
import qualified Tidepool.Command as Cmd

main :: IO ()
main = do
  let source = "0123456789abcdef0123456789abcdef01234567"
      journey = browserFocusedSpec source BrowserJourney
      standalone = browserFocusedSpec source StandaloneReopen
  unless (focusedSource journey == source && focusedSource standalone == source)
    (error "scenario lost the exact source")
  unless (focusedTarget journey == "test:browser_journey"
      && focusedFilter journey == "browser_journey_auth_pending_cancel_child_failure_and_reconnect"
      && focusedExpected journey == 1)
    (error "journey does not select exactly the production browser test")
  unless (focusedTarget standalone == "test:standalone_browser"
      && focusedFilter standalone == "standalone_missing_assets_and_clean_and_process_loss_reopen"
      && focusedExpected standalone == 1)
    (error "standalone does not select exactly the reconnect and reopen test")
  unless (Cmd.commandArgv (Cmd.describe browserPreparationCommand)
      == ["nix", "develop", ".#web", "-c", "bash", "-lc",
          "cd web && npm ci && npm run check && npm test && npm run build"])
    (error "browser preparation drifted from the required web steps")
  unless (Cmd.commandArgv (Cmd.describe browserReadinessCommand)
      == ["test", "-f", "web/dist/index.html"])
    (error "browser readiness must inspect the prepared asset")
