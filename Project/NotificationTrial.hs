{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- Shadow routing reuses the reminder actor's bounded judgments and history.
-- The existing notice policy still owns every outgoing notice.
module Project.NotificationTrial
  ( NotificationDecision (..), notificationDecision, notificationPolicy
  , ShadowWork, trialWork, trialJudgments, followWorkShadow, followWorkShadowWith
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import Tidepool.Actors.Exomonad (ActorHandle, AgentRef, Response, Progress)
import Tidepool.Effects.Core (Actor)
import qualified Data.Text as Text
import qualified Tidepool.Actor.Record as R
import Tidepool.Worktree (renderGitOid)
import Project.Routing
import Project.Types
import Project.WorkflowReminders

data NotificationDecision = RecordOnly | InterruptOwner | RoutingUncertain Text
  deriving (Show, Eq)

notificationDecision :: ReminderEntry -> NotificationDecision
notificationDecision entry = case reminderDecision entry of
  Suggest -> InterruptOwner
  NotApplicable -> RecordOnly
  ReminderUnresolved reason -> RoutingUncertain reason

notificationPolicy :: Text -> ReminderPolicy
notificationPolicy taskIntent = ReminderPolicy
  { reminderContext = taskIntent
  , reminderTrigger = "The incoming event contains a new decision, unresolved question, conflicting evidence, failure, or useful unincorporated result needing the owner's attention. Any such content makes a mixed stale/new message actionable."
  , reminderExclusions = "Record only if ALL meaningful content repeats facts already explicitly handled in the supplied collector state. An acknowledgment is not evidence of incorporation. Missing context, missing source, or uncertain novelty must remain unresolved. Do not infer completion from silence or prose claiming acceptance."
  , reminderSuggestion = "Interrupt the owner with this event and its retained evidence. This is a shadow routing proposal; it changes no delivery or authority."
  , reminderEpisodeLimit = 16
  }

-- Each collector gets its own episode namespace and bounded reminder actor.
-- Retain both handles: finishWork and R.finish release their respective actors.
data ShadowWork value = ShadowWork
  { trialWork :: ActorHandle (WorkActor value)
  , trialJudgments :: ReminderTrial
  } deriving (Show)

followWorkShadow
  :: Member Actor effects
  => AgentRef -> ReminderPolicy
  -> [(Text, Response value, Progress WorkProgress)]
  -> (value -> Text)
  -> Eff effects (Either ReminderIssue (ShadowWork value))
followWorkShadow = followWorkShadowWith semanticReminder

followWorkShadowWith
  :: Member Actor effects
  => ReminderChoice -> AgentRef -> ReminderPolicy
  -> [(Text, Response value, Progress WorkProgress)]
  -> (value -> Text)
  -> Eff effects (Either ReminderIssue (ShadowWork value))
followWorkShadowWith choose owner policy inputs render = do
  admitted <- startReminderTrialWith choose owner policy
  case admitted of
    Left issue -> pure (Left issue)
    Right trial -> do
      collector <- followWork inputs (shadowWorkNotifications trial render (notifyWork owner (workMessage render)))
      pure (Right (ShadowWork collector trial))

-- R.get reads the collector's state after it has retained the incoming event.
-- Use its notice policy to compare the exact text sent by notifyWork.
shadowWorkNotifications
  :: ReminderTrial -> (value -> Text) -> WorkSink value -> WorkSink value
shadowWorkNotifications trial render sink event = do
  state <- R.get
  let excerpt = workNoticeMessage (workNoticePolicy state) (workMessage render) event
      key = "work-event:" <> Text.pack (show (length (workHistory state) - 1))
      episode message = ReminderEpisode key
        (Text.unlines
          [ "Incoming event: " <> message
          , "Incoming candidate facts: " <> case event of
              WorkChanged _ delta -> Text.intercalate "; " (map candidateFacts (addedEvidence delta ++ map checkpointCandidate (addedReviewed delta)))
              _ -> "none"
          , "Explicitly incorporated candidates: " <> Text.intercalate "; "
              [name <> "@" <> candidateFacts candidate
              | (name, candidate) <- handledWork state]
          , "Unresolved questions: " <> Text.intercalate "; "
              [sourceName source <> ": " <> questionKey question <> ": "
                <> questionFinding (questionDetails question)
              | source <- collectedWork state
              , question <- workQuestions (sourceProgress source)]
          ])
        ("Original event retained in workHistory at " <> key
          <> "; exact responses remain in collectedWork. No omitted excerpt establishes completion.")
  case excerpt of
    Nothing -> sink event
    Just message -> withReminders (trialReminders trial) (const (Just (episode message))) sink event

-- A repeated OID can carry changed check claims or remaining gates.
candidateFacts :: Candidate -> Text
candidateFacts candidate = renderGitOid (candidateCommit candidate)
  <> " reported checks: " <> Text.pack (show (checkedCommands candidate))
  <> " remaining gates: " <> Text.pack (show (remainingGates candidate))
