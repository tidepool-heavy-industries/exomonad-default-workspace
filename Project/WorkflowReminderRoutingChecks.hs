{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.WorkflowReminderRoutingChecks (questionRouting, shadowRouting) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import qualified Data.Text as Text
import Tidepool.Check
import Project.Checks (script)

-- A real followWork sink keeps its ordinary notification receipts while the
-- reminder actor sees only newly opened, source-identified question deltas.
questionRouting :: Member RecipeCheck effects => Eff effects ()
questionRouting = do
  owner <- root
  source <- git owner ["rev-parse", "HEAD"]
  script owner "progress-route-producer"
  producer <- activation
  script owner "progress-route-consumer"
  consumer <- activation
  void $ turn owner (Text.unlines
    [ "import qualified Data.Text as Text"
    , "import qualified Tidepool.Actor.Record as R"
    , "let source = " <> gitOidLiteral source
    , "Right reminders <- startRemindersWith (\\_ _ -> pure Suggest) me reviewRelayPolicy"
    , "routed <- followWork [(\"producer\", producer, updates)] (withQuestionReminders reminders (notifyWork (responseActor consumer) (workMessage id)))"
    ])
  void $ turn (checkActor producer) (Text.unlines
    [ "let first = Question \"relay\" (DesignQuestion \"plans/review.md\" " <> gitOidLiteral source <> " \"Manual within-contract review relay\" [\"review result for exact candidate\"] [] [\"repair\"] )"
    , "let second = Question \"ownership\" (DesignQuestion \"plans/review.md\" " <> gitOidLiteral source <> " \"Shared ownership changed\" [\"new owner question\"] [] [\"parent\"] )"
    , "reportProgress (WorkProgress [] [first])"
    ])
  first <- turn owner (Text.unlines
    [ "view <- readWork routed"
    , "memory <- R.call (reminderRead (R.client reminders)) ()"
    , "inspectFull (length (workNotices view) == 1 && length (reminderEntries memory) == 1"
    , "  && map noticeEvent (workNotices view) == [0]"
    , "  && length [() | Notice _ (Left NotificationUnavailable) <- workNotices view] == 1"
    , "  && all (\\entry -> reminderDecision entry == Suggest && (case reminderReceipt entry of { Just (Left NotificationUnavailable) -> True; _ -> False })) (reminderEntries memory)"
    , "  && all (\\entry -> \"plans/review.md#relay@\" `Text.isInfixOf` reminderFacts (reminderEpisode entry)"
    , "    && \"review result for exact candidate\" `Text.isInfixOf` reminderEvidence (reminderEpisode entry)) (reminderEntries memory))"
    ])
  check "opened question retains both typed failed sends and one reminder" ("True" `Text.isSuffixOf` output first)
  pending <- turn (checkActor producer) "import Tidepool.Agent.Reply (pollReply)\npollReply sessionReply"
  check "routing leaves the worker's original reply pending" ("ReplyOpen" `Text.isSuffixOf` output pending)

  void $ turn (checkActor producer) "reportProgress (WorkProgress [] [first])"
  duplicate <- turn owner "view <- readWork routed\nmemory <- R.call (reminderRead (R.client reminders)) ()\ninspectFull (length (workNotices view) == 1 && length (reminderEntries memory) == 1)"
  check "same progress publication produces no duplicate notice or judgment" ("True" `Text.isSuffixOf` output duplicate)

  void $ turn (checkActor producer) "reportProgress (WorkProgress [] [first,second])"
  changed <- turn owner (Text.unlines
    [ "view <- readWork routed"
    , "memory <- R.call (reminderRead (R.client reminders)) ()"
    , "inspectFull (length (workNotices view) == 2 && length (reminderEntries memory) == 2"
    , "  && any (\\entry -> \"plans/review.md#ownership@\" `Text.isInfixOf` reminderFacts (reminderEpisode entry)) (reminderEntries memory))"
    ])
  check "new source-identified question receives its own episode" ("True" `Text.isSuffixOf` output changed)

  void $ turn (checkActor producer) "reportProgress (WorkProgress [] [])"
  resolved <- turn owner "view <- readWork routed\nmemory <- R.call (reminderRead (R.client reminders)) ()\ninspectFull (length (workNotices view) == 3 && length (reminderEntries memory) == 2)"
  check "question resolution keeps ordinary sink notice without another reminder" ("True" `Text.isSuffixOf` output resolved)
  void $ turn (checkActor producer) "respond (\"finished\" :: Text)"
  terminal <- turn owner "view <- readWork routed\nmemory <- R.call (reminderRead (R.client reminders)) ()\ninspectFull (length (workNotices view) == 4 && length (reminderEntries memory) == 2)"
  check "terminal result keeps ordinary sink notice without a reminder" ("True" `Text.isSuffixOf` output terminal)
  void $ turn (checkActor consumer) "respond (\"done\" :: Text)"
  void $ turn owner "finishWork routed\nR.finish reminders"

-- Exercise the shadow sink against actual collector state, with deterministic
-- judgment so this regression needs no provider or credits.
shadowRouting :: Member RecipeCheck effects => Eff effects ()
shadowRouting = do
  owner <- root
  source <- git owner ["rev-parse", "HEAD"]
  script owner "progress-route-producer"
  producer <- activation
  void $ turn owner (Text.unlines
    [ "import qualified Data.Text as T"
    , "import Project.NotificationTrial"
    , "import Project.WorkflowReminders"
    , "import qualified Tidepool.Actor.Record as R"
    , "Right experiment <- followWorkShadowWith (\\_ _ -> pure Suggest) me (notificationPolicy \"Integrate reviewed component slices\") [(\"producer\", producer, updates)] id"
    , "let trial = trialJudgments experiment"
    , "let routed = trialWork experiment"
    ])
  void $ turn (checkActor producer) (Text.unlines
    [ "let question = Question \"ownership\" (DesignQuestion \"plans/component.md\" " <> gitOidLiteral source <> " \"Old status repeated, but who owns the new file?\" [\"ownership not assigned\"] [] [\"parent\"])"
    , "reportProgress (WorkProgress [Candidate " <> gitOidLiteral source <> " [\"unit-one\"] [\"browser gate remains\"]] [question])"
    ])
  observed <- turn owner (Text.unlines
    [ "view <- readWork routed"
    , "memory <- R.call (reminderRead (R.client (trialReminders trial))) ()"
    , "inspectFull (length (workNotices view) == 1 && length (reminderEntries memory) == 1"
    , "  && all (\\entry -> (case reminderReceipt entry of { Nothing -> True; Just _ -> False }) && notificationDecision entry == InterruptOwner) (reminderEntries memory)"
    , "  && all (\\entry -> \"Unresolved questions: producer: ownership\" `T.isInfixOf` reminderFacts (reminderEpisode entry) && \"browser gate remains\" `T.isInfixOf` reminderFacts (reminderEpisode entry)) (reminderEntries memory))"
    ])
  check "shadow routing uses actual unresolved questions and preserves original notice" ("True" `Text.isSuffixOf` output observed)
  void $ turn (checkActor producer) "reportProgress (WorkProgress [] [question])"
  duplicate <- turn owner "memory <- R.call (reminderRead (R.client (trialReminders trial))) ()\ninspectFull (length (reminderEntries memory))"
  check "unchanged progress does not produce another shadow judgment" (lastOutput duplicate == "1")
  void $ turn (checkActor producer) "respond (\"done\" :: Text)"
  void $ turn owner "finishWork routed\nR.finish (trialReminders trial)"
