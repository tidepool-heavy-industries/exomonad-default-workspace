{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | Exact source, artifact and test-count evidence for a focused Cargo check.
module Project.TestEvidence
  ( FocusedSpec (..), FocusedSetupIssue (..), FocusedRun (..), FocusedRecord (..), FailureKind (..)
  , FocusedResult (..), startFocused, collectFocused, finishFocused, focusedPassed
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Jev.Operators as J
import Jev.Operators (Packet ((:=), (:&)))
import qualified Tidepool.Command as Cmd
import Tidepool.Aeson (FromJSON (..), (.:), (.:?), withObject)
import Tidepool.Effects.Core (Commands, Jev)
import Tidepool.QQ.Bash (bash)

data FocusedSpec = FocusedSpec
  { focusedIntent :: Text
  , focusedSource :: Text
  , focusedPackage :: Text
  , focusedTarget :: Text
  , focusedFilter :: Text
  , focusedExpected :: Int
  } deriving (Show, Eq)

data FocusedRun = FocusedRun FocusedSpec Cmd.Job deriving (Show)

data FocusedSetupIssue = NonPositiveExpected Int deriving (Show, Eq)

-- The runner writes these fields to evidence.json before test execution and
-- fills counts as they become known. Missing fields stay missing evidence.
data FocusedRecord = FocusedRecord
  { recordSource :: Maybe Text
  , recordWorkingTree :: Maybe Text
  , recordExecutable :: Text
  , recordDigest :: Text
  , recordOutput :: Text
  , recordRunnable :: Maybe [Text]
  , recordSummaries :: Maybe [[Int]]
  , recordExitCode :: Maybe Int
  } deriving (Show, Eq)

instance FromJSON FocusedRecord where
  parseJSON = withObject "focused test evidence" $ \fields ->
    FocusedRecord <$> fields .: "source"
      <*> fields .: "working_tree_status"
      <*> fields .: "executable"
      <*> fields .: "sha256"
      <*> fields .: "output"
      <*> fields .:? "runnable"
      <*> fields .:? "summaries"
      <*> fields .:? "exit_code"

data FailureKind
  = ImplementationFailure
  | FixtureFailure
  | MissingPrerequisite
  | InsufficientEvidence
  deriving (Show, Eq)

data FocusedResult = FocusedResult
  { focusedCommand :: Cmd.RunResult
  , focusedEvidencePath :: Maybe Text
  , focusedEvidence :: Either Text FocusedRecord
  , focusedFailure :: Maybe (Either Text FailureKind)
  } deriving (Show)

-- Run in the actor's checkout. The caller chooses a realistic memory limit for
-- this package; the background job retains its source and terminal receipt.
startFocused :: Member Commands effects => Cmd.Memory -> FocusedSpec -> Eff effects (Either FocusedSetupIssue FocusedRun)
startFocused memory spec
  | focusedExpected spec <= 0 = pure (Left (NonPositiveExpected (focusedExpected spec)))
  | otherwise = do
    job <- Cmd.background $ Cmd.withMemory memory $
      Cmd.withArguments
        [ focusedPackage spec, focusedTarget spec, focusedFilter spec
        , Text.pack (show (focusedExpected spec)) ]
        [bash|set -euo pipefail
scripts/cargo-focused-test --package "$1" --target "$2" --filter "$3" --expect "$4"|]
    pure (Right (FocusedRun spec job))

-- Read retained evidence after completion, without a model judgment.
collectFocused :: Member Commands effects => FocusedRun -> Eff effects FocusedResult
collectFocused (FocusedRun _ job) = do
  completed <- Cmd.await job
  let path = evidencePath (Cmd.stderr completed)
  evidence <- case path of
    Nothing -> pure (Left "focused runner did not report an absolute evidence.json path")
    Just file -> do
      loaded <- Cmd.run (Cmd.argv ["cat", file])
      pure $ if Cmd.failure loaded /= Nothing
          || Cmd.commandCleanup (Cmd.commandResult loaded) /= Cmd.CommandClean
        then Left ("cannot read focused evidence; cat receipt: " <>
          Text.pack (show (Cmd.commandResult loaded)))
        else case Cmd.decodeWith (Cmd.asJSON @FocusedRecord) (Cmd.stdout loaded) of
          Left issue -> Left ("cannot decode focused evidence: " <> Text.pack (show issue))
          Right record -> Right record
  pure (FocusedResult completed path evidence Nothing)

-- An optional diagnosis of a failed check never changes its pass rule.
finishFocused
  :: (Member Commands effects, Member Jev effects)
  => FocusedRun -> Eff effects FocusedResult
finishFocused run@(FocusedRun spec _) = do
  result <- collectFocused run
  judgment <- case (Cmd.failure (focusedCommand result), focusedEvidence result) of
    (Nothing, _) -> pure Nothing
    (_, Left _) -> pure Nothing
    (Just _, Right record) -> do
      excerpt <- Cmd.run (Cmd.argv ["tail", "-n", "80", recordOutput record])
      let diagnostic = Text.takeEnd 8000 $ Cmd.stderr (focusedCommand result) <> "\n" <>
            either (const "output log unavailable") id (Cmd.stdout excerpt)
      answer <- J.ask1
        (J.state (#intent := focusedIntent spec :& #diagnostic := diagnostic))
        (J.choice "Which explanation best fits this failed focused check?"
          (J.alt #implementation "The assertion or compiler diagnostic points to the implementation" ImplementationFailure
            J..| J.alt #fixture "The failure points to test setup or fixture data" FixtureFailure
            J..| J.alt #prerequisite "A missing tool, dependency or environment condition prevented the check" MissingPrerequisite
            J..| J.alt #insufficient "The retained diagnostic does not establish any of those causes" InsufficientEvidence))
      pure $ Just $ case answer of
        Left issue -> Left (Text.pack (show issue))
        Right choice -> case J.takenUnder J.lenient choice of
          Left doubt -> Left doubt.why
          Right (J.Settled kind) -> Right kind
  pure result {focusedFailure = judgment}

-- Exit, checkout identity, selected count and executed count are code facts.
-- A Jev classification is never an input to this predicate.
focusedPassed :: FocusedSpec -> FocusedResult -> Bool
focusedPassed spec result = case focusedEvidence result of
  Left _ -> False
  Right record ->
    focusedExpected spec > 0
      && Cmd.failure (focusedCommand result) == Nothing
      && Cmd.commandCleanup (Cmd.commandResult (focusedCommand result)) == Cmd.CommandClean
      && recordSource record == Just (focusedSource spec)
      && recordWorkingTree record == Just ""
      && recordExitCode record == Just 0
      && maybe False ((== focusedExpected spec) . length) (recordRunnable record)
      && case recordSummaries record of
        Just [[passed, failed, _, _, _]] -> passed == focusedExpected spec && failed == 0
        _ -> False

evidencePath :: Text -> Maybe Text
evidencePath stderr = case
  [ Text.strip (Text.drop (Text.length marker) line)
  | line <- Text.lines stderr, marker `Text.isPrefixOf` line ] of
    path : _ | "/" `Text.isPrefixOf` path -> Just path
    _ -> Nothing
  where marker = "focused test evidence: "
