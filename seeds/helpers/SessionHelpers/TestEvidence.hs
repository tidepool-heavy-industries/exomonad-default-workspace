{-# LANGUAGE FlexibleContexts #-}

-- | Remix this session seed for the current component. Keep evidence parsing
-- and acceptance rules in the shared owner; specialize commands and policy here.
module SessionHelpers.TestEvidence
  ( module Project.TestEvidence, runTests, CheckDefinition (..), checkAt, runCheck
  , CheckPreparation (..), PlanCheck (..), PlanStart (..), PlanReport (..)
  , startCheckPlan, readCheckPlan, planPassed, planSummary
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import qualified Data.Text as Text
import Project.CheckResults
  ( CheckActor, CheckEntry (..), CheckSetupIssue (..), CheckState (..), CheckVerdict (..)
  , NoticePolicy (NotifySummary), checkVerdict, readChecks
  , watchChecksWithRefusals )
import Project.FocusedGateExample (GateStart, startGate)
import Project.TestEvidence
import qualified Tidepool.Actor.Record as R
import Tidepool.Actors.Exomonad (AgentRef)
import qualified Tidepool.Command as Cmd
import Tidepool.Effects.Core (Actor, Commands)
import Tidepool.Worktree (GitOid, renderGitOid)

-- Remix the definition; supply the committed candidate at each invocation.
data CheckDefinition = CheckDefinition
  { checkIntent :: Text, checkPackage :: Text, checkTarget :: Text
  , checkFilter :: Text, checkExpected :: Int
  } deriving (Show, Eq)

checkAt :: GitOid -> CheckDefinition -> FocusedSpec
checkAt candidate definition = FocusedSpec
  (checkIntent definition) (renderGitOid candidate) (checkPackage definition)
  (checkTarget definition) (checkFilter definition) (checkExpected definition)

runCheck :: (Member Actor effects, Member Commands effects) => AgentRef -> GitOid -> Cmd.Memory -> CheckDefinition -> Eff effects GateStart
runCheck owner candidate memory definition =
  startGate owner (checkIntent definition) memory (checkAt candidate definition)

-- Start once, retain the result, then compose watchChecks or collectFocused.
-- Tune this reservation and specialize a FocusedSpec for the current work.
runTests
  :: Member Commands effects
  => FocusedSpec -> Eff effects (Either FocusedSetupIssue FocusedRun)
runTests = startFocused (Cmd.GiB 4)

-- | A small acceptance plan over exact candidate checks. Preparation runs in
-- the same original job as its check; each item chooses its own reservation.
data CheckPreparation = WithoutPreparation | PrepareWith (GitOid -> [Text])

data PlanCheck = PlanCheck
  { planDefinition :: CheckDefinition
  , planMemory :: Cmd.Memory
  , planPreparation :: CheckPreparation
  }

data PlanStart = PlanStart
  { planStarts :: [(Text, Either FocusedSetupIssue FocusedRun)]
  , planWatcher :: Maybe (Either CheckSetupIssue (R.ActorHandle CheckActor))
  } deriving (Show)

data PlanReport = PlanReport
  { planOriginal :: PlanStart
  , planState :: Maybe CheckState
  } deriving (Show)

-- | Validate names before submission, then retain every start result. The
-- single watcher observes admitted jobs and includes start refusals in its
-- terminal notice. It never submits a replacement for a refused check.
startCheckPlan
  :: (Member Actor effects, Member Commands effects)
  => AgentRef -> GitOid -> [PlanCheck]
  -> Eff effects (Either CheckSetupIssue PlanStart)
startCheckPlan owner candidate checks = case checks of
  [] -> pure (Left NoFocusedChecks)
  _ | Just name <- duplicateName names -> pure (Left (DuplicateCheckName name))
  _ -> do
    launched <- forM checks $ \item -> do
      let definition = planDefinition item
          spec = checkAt candidate definition
          memory = planMemory item
      result <- case planPreparation item of
        WithoutPreparation -> startFocused memory spec
        PrepareWith commandFor -> startFocusedAfter memory spec (commandFor candidate)
      pure (checkIntent definition, result)
    let admitted = [(name, run) | (name, Right run) <- launched]
        refused = [(name, issue) | (name, Left issue) <- launched]
    watcher <- case admitted of
      [] -> pure Nothing
      _ -> Just <$> watchChecksWithRefusals owner NotifySummary refused admitted
    pure (Right (PlanStart launched watcher))
  where names = map (checkIntent . planDefinition) checks

duplicateName :: [Text] -> Maybe Text
duplicateName [] = Nothing
duplicateName (name : rest)
  | name `elem` rest = Just name
  | otherwise = duplicateName rest

readCheckPlan :: Member Actor effects => PlanStart -> Eff effects PlanReport
readCheckPlan started = do
  state <- case planWatcher started of
    Just (Right watcher) -> Just <$> readChecks watcher
    _ -> pure Nothing
  pure (PlanReport started state)

-- Every requested check must have started, completed and passed. The watcher
-- admission refusal and incomplete observation are visible as non-passing.
planPassed :: PlanReport -> Bool
planPassed report = case planWatcher (planOriginal report) of
  Just (Right _) -> not (null statuses) && all (== Just CheckPassed) statuses
  _ -> False
  where statuses = map (snd . planStatus report) (planStarts (planOriginal report))

planSummary :: PlanReport -> Text
planSummary report =
  "check plan " <> (if planPassed report then "passed" else "not passed")
    <> (if any (either (const True) (const False) . snd)
          (planStarts (planOriginal report))
        then " (not all requested checks ran)" else "")
    <> ": " <> Text.intercalate "; "
      [name <> ": " <> case result of
        Left issue -> "start refused (" <> Text.pack (show issue) <> ")"
        Right _ -> maybe "running or unavailable" (Text.pack . show) verdict
      | (name, result) <- planStarts (planOriginal report)
      , let verdict = snd (planStatus report (name, result))]
    <> case planWatcher (planOriginal report) of
      Just (Left issue) -> "; watcher refused (" <> Text.pack (show issue) <> ")"
      _ -> ""

planStatus :: PlanReport -> (Text, Either FocusedSetupIssue FocusedRun) -> (Text, Maybe CheckVerdict)
planStatus report (name, result) = (name, case (result, planState report) of
  (Right _, Just state) -> case [entry | entry <- checkEntries state, checkName entry == name] of
    entry : _ -> checkVerdict entry <$> checkOutcome entry
    [] -> Nothing
  _ -> Nothing)
