{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- Native handlers execute the exported workflow; the script rejects every
-- unexpected command effect, including background starts and timed observations.
module Main (main) where

import Control.Exception (ErrorCall, Exception, throwIO, try)
import Control.Monad (forM_, unless)
import Control.Monad.Freer (Eff, interpretM, runM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Project.BrowserScenario
import Project.TestEvidence
import qualified Tidepool.Command as Cmd
import Tidepool.Command.Types (Job (..))
import Tidepool.Effects.Core (Commands (..))
import Tidepool.Agent.Reply.Internal (ReplyError (..))
import Tidepool.Agent.Watch.Internal
  ( Watches (..), AwaitPlan (..), AwaitNode (..), AwaitDependency (..)
  , AwaitDecision (..), RawWatchObservation (..), ForgetWatchOutcome (..) )

data Step
  = Start Cmd.CommandSpec (Either Cmd.CommandError Text)
  | Background Cmd.CommandSpec Text
  | Wait Text (Either ReplyError TerminalFixture)

data ProtocolFailure = ProtocolFailure String deriving (Show)
instance Exception ProtocolFailure

-- The fixture records terminal evidence and an independent bounded output read.
type TerminalFixture = (Cmd.CommandResult, Either Cmd.CommandError Cmd.CommandOutput)

data CompletionPhase = Waiting | Ready | Projected | Released deriving (Eq)

execute :: [Step] -> Eff '[Commands, Watches, IO] value -> IO value
execute expected action = do
  remaining <- newIORef expected
  executeScript remaining action

executeScript :: IORef [Step] -> Eff '[Commands, Watches, IO] value -> IO value
executeScript remaining action = do
  pending <- newIORef (Nothing :: Maybe (Text, TerminalFixture, CompletionPhase))
  let commandHandler :: Commands value -> IO value
      commandHandler request = case request of
        CommandOutputWith actual budget -> do
          retained <- readIORef pending
          case retained of
            Just (wanted, terminal, Released) -> do
              assert "bounded output follows shared readiness and release" (actual == wanted && budget == 1024 * 1024)
              writeIORef pending Nothing
              pure (snd terminal)
            _ -> throwIO (ProtocolFailure "output requested without an awaited completion")
        _ -> do
          steps <- readIORef remaining
          case (steps, request) of
            (Start wanted answer : rest, CommandStartWith actual) -> do
              assert "exact invocation-owned command" (actual == wanted)
              writeIORef remaining rest
              pure answer
            (Background wanted key : rest, CommandBackgroundWith actual) -> do
              assert "existing actor-owned helper shares command builder" (actual == wanted)
              writeIORef remaining rest
              pure (Right key)
            _ -> throwIO (ProtocolFailure "unexpected command effect or command ordering")
      watchHandler :: Watches value -> IO value
      watchHandler request = case request of
        RegisterAwaitWith (AwaitPlan [LeafNode (AwaitCommand actual)] 0) -> do
          prior <- readIORef pending
          assert "previous completion and output were consumed" (case prior of Nothing -> True; _ -> False)
          steps <- readIORef remaining
          case steps of
            Wait wanted answer : rest -> do
              assert "await exact retained job" (actual == wanted)
              writeIORef remaining rest
              case answer of
                Left issue -> pure (Left issue)
                Right terminal -> writeIORef pending (Just (wanted, terminal, Waiting)) >> pure (Right 1)
            _ -> throwIO (ProtocolFailure "unscripted shared await")
        AwaitWatchWith 1 -> do
          retained <- readIORef pending
          case retained of
            Just (key, terminal, Waiting) -> do
              writeIORef pending (Just (key, terminal, Ready))
              pure (RawWatchReady (AwaitDecision [(0, Nothing)] []))
            _ -> throwIO (ProtocolFailure "await without registered command")
        ObserveWatchCommandWith 1 [] actual -> do
          retained <- readIORef pending
          case retained of
            Just (wanted, terminal, Ready) -> do
              assert "project exact retained command" (actual == wanted)
              writeIORef pending (Just (wanted, terminal, Projected))
              pure (Just (Cmd.CommandCompletion wanted [] Nothing (fst terminal)))
            _ -> throwIO (ProtocolFailure "projection before readiness")
        ForgetWatchWith 1 -> do
          retained <- readIORef pending
          case retained of
            Just (key, terminal, Projected) -> do
              writeIORef pending (Just (key, terminal, Released))
              pure WatchForgotten
            _ -> throwIO (ProtocolFailure "subscription released before projection")
        _ -> throwIO (ProtocolFailure "unexpected shared watch operation")
  result <- runM (interpretM watchHandler (interpretM commandHandler action))
  steps <- readIORef remaining
  retained <- readIORef pending
  assert "all scripted effects occurred" (null steps && case retained of Nothing -> True; _ -> False)
  pure result

assert :: String -> Bool -> IO ()
assert label condition = unless condition (throwIO (ProtocolFailure label))

workspace :: BrowserWorkspace
workspace = BrowserWorkspace "/tmp/harness-checkout" "0123456789abcdef0123456789abcdef01234567"

resources :: BrowserResources
resources = BrowserResources (Cmd.GiB 4) (Cmd.MiB 1536)

prepareSpec, readySpec :: Cmd.CommandSpec
prepareSpec = Cmd.describe $ Cmd.withMemory (preparationMemory resources) $
  Cmd.inDirectory (browserCheckout workspace) (browserPreparationCommand workspace)
readySpec = Cmd.describe $ Cmd.withMemory (preparationMemory resources) $
  Cmd.inDirectory (browserCheckout workspace) (browserReadinessCommand workspace)

page :: Text -> Cmd.CommandPage
page text = Cmd.CommandPage text 0 size size 0 0 True False False False
  where size = Text.length text

observation :: Cmd.CommandOutcome -> Cmd.CommandCleanup -> Either Cmd.CommandError Cmd.CommandOutput -> TerminalFixture
observation outcome cleanup output = (Cmd.CommandResult outcome cleanup, output)

successful :: TerminalFixture
successful = observation (Cmd.CommandExited 0) Cmd.CommandClean (Right (Cmd.CommandOutput (page "") (page "")))

receipt :: Text -> TerminalFixture -> Cmd.RunResult
receipt key observed = Cmd.Finished (Job key) (fst observed) (snd observed)

prerequisites :: [Step]
prerequisites = [Start prepareSpec (Right "preparation"), Wait "preparation" (Right successful),
  Start readySpec (Right "readiness"), Wait "readiness" (Right successful)]

-- Obtain the existing owner's actual command and compare both start variants.
-- The workflow tests also check checkout/resources/argv explicitly below.
focusedSpecFor :: BrowserScenario -> IO Cmd.CommandSpec
focusedSpecFor scenario = do
  captured <- newIORef Nothing
  let handler :: Commands value -> IO value
      handler (CommandBackgroundWith command) = writeIORef captured (Just command) >> pure (Right "capture")
      handler _ = throwIO (ProtocolFailure "focused actor helper emitted unexpected effect")
  started <- runM $ interpretM handler $ startFocusedIn (browserCheckout workspace)
    (focusedMemory resources) (browserFocusedSpec workspace scenario)
  assert "actor helper retains requested spec and job" $ case started of
    Right run -> runSpec run == browserFocusedSpec workspace scenario && runJob run == Job "capture"
    _ -> False
  found <- readIORef captured
  case found of
    Just command -> do
      assert "focused checkout and memory" (Cmd.commandDirectory command == Just (browserCheckout workspace)
        && Cmd.commandMemory command == 1536 * 1024 * 1024 && Cmd.commandInput command == Cmd.ClosedInput)
      assert "focused exact runner selection" (drop 6 (Cmd.commandArgv command) ==
        ["harness-demo", focusedTarget (browserFocusedSpec workspace scenario),
          focusedFilter (browserFocusedSpec workspace scenario), "1", "0", "scripts/cargo-focused-test"])
      pure command
    Nothing -> throwIO (ProtocolFailure "focused command not captured")

focusedObservation :: BrowserScenario -> TerminalFixture
focusedObservation scenario = observation (Cmd.CommandExited 0) Cmd.CommandClean $ Right $
  Cmd.CommandOutput (page "") (page evidence)
  where
    name = focusedFilter (browserFocusedSpec workspace scenario)
    evidence = "focused test evidence: /tmp/evidence.json\nfocused test record begin\n"
      <> "{\"source\":\"" <> browserSource workspace
      <> "\",\"working_tree_status\":\"\",\"executable\":\"/tmp/test\",\"sha256\":\"digest\",\"output\":\"/tmp/output\","
      <> "\"matched\":[\"" <> name <> "\"],\"runnable\":[\"" <> name <> "\"],\"summaries\":[[1,0,0,0,0]],\"exit_code\":0}"
      <> "\nfocused test record end\nfocused test record status: available\n"

main :: IO ()
main = do
  let preparation = Cmd.commandArgv prepareSpec
      readiness = Cmd.commandArgv readySpec
  assert "preparation exact source guard and mandatory web steps" $
    length preparation == 9
      && take 6 preparation == ["nix", "develop", ".#web", "-c", "bash", "-lc"]
      && drop 7 preparation == ["browser-prepare", browserSource workspace]
      && all (`Text.isInfixOf` (preparation !! 6)) ["npm ci", "npm run check", "npm test", "npm run build"]
      && Text.count "git rev-parse HEAD" (preparation !! 6) == 2
      && Text.count "git status --porcelain" (preparation !! 6) == 2
      && Cmd.commandDirectory prepareSpec == Just (browserCheckout workspace)
      && Cmd.commandMemory prepareSpec == 4 * 1024 * 1024 * 1024
  assert "readiness exact source and asset checks" $
    length readiness == 5 && take 2 readiness == ["bash", "-lc"]
      && drop 3 readiness == ["browser-ready", browserSource workspace]
      && all (`Text.isInfixOf` (readiness !! 2))
        ["git rev-parse HEAD", "git status --porcelain", "test -f web/dist/index.html"]
      && Cmd.commandDirectory readySpec == Just (browserCheckout workspace)
      && Cmd.commandMemory readySpec == Cmd.commandMemory prepareSpec
  let journey = browserFocusedSpec workspace BrowserJourney
      standalone = browserFocusedSpec workspace StandaloneReopen
  assert "production scenarios retain exact source and counts" $
    focusedSource journey == browserSource workspace && focusedSource standalone == browserSource workspace
      && focusedTarget journey == "test:browser_journey"
      && focusedFilter journey == "browser_journey_auth_pending_cancel_child_failure_and_reconnect"
      && focusedTarget standalone == "test:standalone_browser"
      && focusedFilter standalone == "standalone_missing_assets_and_clean_and_process_loss_reopen"
      && focusedExpected journey == 1 && focusedExpected standalone == 1
  invalid <- execute [] $ runBrowserScenarios (workspace { browserCheckout = "relative" }) resources [BrowserJourney]
  assert "invalid directory before effects" $ case invalid of Left (InvalidBrowserCheckout "relative") -> True; _ -> False
  empty <- execute [] $ runBrowserScenarios workspace resources []
  assert "empty selection before effects" $ case empty of Left NoBrowserScenarios -> True; _ -> False
  duplicate <- execute [] $ runBrowserScenarios workspace resources [BrowserJourney, BrowserJourney]
  assert "duplicate selection before effects" $ case duplicate of Left (DuplicateBrowserScenario BrowserJourney) -> True; _ -> False
  refused <- execute [Start prepareSpec (Left Cmd.CommandUnauthorized)] $ runBrowserScenarios workspace resources [BrowserJourney]
  assert "preparation refusal stops downstream" $ case refused of Left (BrowserPreparationRejected Cmd.CommandUnauthorized) -> True; _ -> False
  let failures =
        [ observation (Cmd.CommandExited 9) Cmd.CommandClean (snd successful)
        , observation Cmd.CommandCancelled Cmd.CommandClean (Left Cmd.CommandOutputPending)
        , observation (Cmd.CommandUnconfirmed "exit unknown") Cmd.CommandClean (Left Cmd.CommandOutputPending)
        , observation (Cmd.CommandExited 0) Cmd.CommandRetained (snd successful)
        , observation (Cmd.CommandExited 0) (Cmd.CommandCleanupUnknown "retained") (snd successful)
        ]
  forM_ failures $ \failed -> do
    result <- execute [Start prepareSpec (Right "preparation"), Wait "preparation" (Right failed)] $
      runBrowserScenarios workspace resources [BrowserJourney]
    assert "preparation terminal safety stops readiness and focused starts" $ case result of
      Left (BrowserPreparationFailed actual) -> actual == receipt "preparation" failed
      _ -> False
    resultReady <- execute (take 2 prerequisites ++ [Start readySpec (Right "readiness"), Wait "readiness" (Right failed)]) $
      runBrowserScenarios workspace resources [BrowserJourney]
    assert "readiness failure retains both receipts and stops focused starts" $ case resultReady of
      Left (BrowserReadinessFailed prep actual) -> prep == receipt "preparation" successful && actual == receipt "readiness" failed
      _ -> False
  readinessRefused <- execute (take 2 prerequisites ++ [Start readySpec (Left (Cmd.CommandUnavailable "fixture"))]) $
    runBrowserScenarios workspace resources [BrowserJourney]
  assert "readiness refusal retains preparation" $ case readinessRefused of
    Left (BrowserReadinessRejected prep (Cmd.CommandUnavailable "fixture")) -> prep == receipt "preparation" successful
    _ -> False
  journeyCommand <- focusedSpecFor BrowserJourney
  standaloneCommand <- focusedSpecFor StandaloneReopen
  forM_ [(False, False), (True, False), (False, True), (True, True)] $ \(refuseJourney, refuseStandalone) -> do
    let startAnswer refuse key = if refuse then Left Cmd.CommandUnauthorized else Right key
        focusedStarts = [Start journeyCommand (startAnswer refuseJourney "journey"),
          Start standaloneCommand (startAnswer refuseStandalone "standalone")]
        focusedWaits = [Wait "journey" (Right (focusedObservation BrowserJourney)) | not refuseJourney]
          ++ [Wait "standalone" (Right (focusedObservation StandaloneReopen)) | not refuseStandalone]
    result <- execute (prerequisites ++ focusedStarts ++ focusedWaits) $
      runBrowserScenarios workspace resources [BrowserJourney, StandaloneReopen]
    assert "start all before collection and retain each refusal/result in caller order" $ case result of
      Right completed -> browserPreparation completed == receipt "preparation" successful
        && browserReadiness completed == receipt "readiness" successful
        && case browserResults completed of
          [(BrowserJourney, first), (StandaloneReopen, second)] ->
            matches refuseJourney BrowserJourney "journey" first && matches refuseStandalone StandaloneReopen "standalone" second
          _ -> False
      _ -> False
  reversed <- execute (prerequisites ++ [Start standaloneCommand (Right "standalone"),
    Start journeyCommand (Right "journey"), Wait "standalone" (Right (focusedObservation StandaloneReopen)),
    Wait "journey" (Right (focusedObservation BrowserJourney))]) $
      runBrowserScenarios workspace resources [StandaloneReopen, BrowserJourney]
  assert "preserve caller order for starts and collection" $ case reversed of
    Right completed -> case browserResults completed of
      [(StandaloneReopen, first), (BrowserJourney, second)] ->
        matches False StandaloneReopen "standalone" first && matches False BrowserJourney "journey" second
      _ -> False
    _ -> False
  forM_ [observation (Cmd.CommandExited 9) Cmd.CommandClean (snd successful),
    observation Cmd.CommandCancelled Cmd.CommandRetained (Left Cmd.CommandOutputPending)] $ \failed -> do
      terminal <- execute (prerequisites ++ [Start journeyCommand (Right "journey"),
        Start standaloneCommand (Right "standalone"), Wait "journey" (Right failed),
        Wait "standalone" (Right (focusedObservation StandaloneReopen))]) $
          runBrowserScenarios workspace resources [BrowserJourney, StandaloneReopen]
      assert "failed terminal focused result retained while other selected result is collected" $ case terminal of
        Right completed -> case browserResults completed of
          [(BrowserJourney, Right first), (StandaloneReopen, second)] ->
            focusedCommand first == receipt "journey" failed
              && focusedSpec first == journey && matches False StandaloneReopen "standalone" second
          _ -> False
        _ -> False
  let unavailable = observation (Cmd.CommandExited 0) Cmd.CommandClean (Left Cmd.CommandOutputPending)
  outputRefused <- execute (take 1 prerequisites ++ [Wait "preparation" (Right unavailable),
    Start readySpec (Right "readiness"), Wait "readiness" (Right unavailable),
    Start journeyCommand (Right "journey"), Wait "journey" (Right unavailable)]) $
      runBrowserScenarios workspace resources [BrowserJourney]
  assert "output refusal stays independent of prerequisite and focused outcomes" $ case outputRefused of
    Right completed -> browserPreparation completed == receipt "preparation" unavailable
      && browserReadiness completed == receipt "readiness" unavailable
      && case browserResults completed of
        [(BrowserJourney, Right result)] -> focusedCommand result == receipt "journey" unavailable
          && case focusedEvidence result of Left _ -> True; _ -> False
        _ -> False
    _ -> False
  refusalScript <- newIORef [Start prepareSpec (Right "preparation"), Wait "preparation" (Left ReplyUnauthorized)]
  awaitRefused <- try (executeScript refusalScript $ runBrowserScenarios workspace resources [BrowserJourney])
    :: IO (Either ErrorCall (Either BrowserRunIssue BrowserResult))
  remainingRefusalSteps <- readIORef refusalScript
  assert "await service refusal fails continuation after the exact scripted start and refusal" $
    null remainingRefusalSteps && case awaitRefused of Left _ -> True; _ -> False
  protocolRejected <- try (try (execute [Start prepareSpec (Right "preparation"),
    Wait "wrong-job" (Left ReplyUnauthorized)] $ runBrowserScenarios workspace resources [BrowserJourney])
      :: IO (Either ErrorCall (Either BrowserRunIssue BrowserResult)))
    :: IO (Either ProtocolFailure (Either ErrorCall (Either BrowserRunIssue BrowserResult)))
  assert "wrong-job protocol failure cannot masquerade as expected await refusal" $
    case protocolRejected of Left _ -> True; _ -> False
  emptyRunner <- execute [] $ startFocusedScopedInWith [] (browserCheckout workspace) (focusedMemory resources) journey
  assert "scoped helper rejects empty runner before effects" $ case emptyRunner of Left EmptyRunner -> True; _ -> False
  invalidExpected <- execute [] $ startFocusedScopedIn (browserCheckout workspace) (focusedMemory resources)
    (journey { focusedExpected = 0 })
  assert "scoped helper rejects invalid expected count before effects" $ case invalidExpected of Left (NonPositiveExpected 0) -> True; _ -> False
  relativeFocused <- execute [] $ startFocusedScopedIn "relative" (focusedMemory resources) journey
  assert "scoped helper rejects relative checkout before effects" $ case relativeFocused of Left (NonAbsoluteCheckout "relative") -> True; _ -> False
  _ <- execute [Background journeyCommand "actor-owned"] $
    startFocusedIn (browserCheckout workspace) (focusedMemory resources) journey
  putStrLn "browser-scenario-contract: 24 workflow cases passed; protocol-failure isolation regression and 3 scoped-helper validation cases, command parity and production source/spec checks passed"
  where
    matches refused scenario key result = case (refused, result) of
      (True, Left (FocusedStartRefused Cmd.CommandUnauthorized)) -> True
      (False, Right terminal) -> focusedSpec terminal == browserFocusedSpec workspace scenario
        && focusedCommand terminal == receipt key (focusedObservation scenario)
        && focusedEvidencePath terminal == Just "/tmp/evidence.json"
        && focusedPreparation terminal == NoPreparation
      _ -> False
