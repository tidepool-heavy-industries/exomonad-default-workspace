{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Project.BrowserScenario
import Project.TestEvidence (FocusedSpec (..))

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
