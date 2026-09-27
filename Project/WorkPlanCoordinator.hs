{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Event-driven interpreter for an authored WorkPlan. The actor owns each
-- request it submits. Routing, ReviewFlow, CheckResults, and Merge continue to
-- own their respective histories and checks.
module Project.WorkPlanCoordinator
  ( Coordinator (beginPlan, planView, answerQuestion, correctQuestion, observeCorrection, reportIncorporation, closePlan)
  , PlanView (..), ActiveDevelopment (..), CoordinatorStart (..), QuestionReply (..)
  , PlanClose (..), CorrectionReply (..), CorrectionState (..)
  , coordinator
  ) where

import Control.Monad.Freer (Eff, Member)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Tidepool.Actor as Actor
import qualified Tidepool.Actor.Record as R
import Tidepool.Actors.Exomonad
import Tidepool.Effects.Core (Actor)
import Tidepool.Effects.Row (knownEffects)
import Project.CheckResults (CheckState, finishChecks)
import Project.BaselineIncorporation
  ( BaselineChange (..), Affected (..), validateBaselineFor, incorporationUpdate )
import Project.FocusedGateExample
  ( PlanReport (..), planPassed, planSummary, startCheckPlanInto )
import qualified Project.FocusedGateExample as Focused
import qualified Project.Merge as Merge
import Project.ReviewFlow
  ( ReviewFlow, ReviewCompletion (..), ReviewFlowState (..), ReviewFlowPolicy (..)
  , checkedReviewFlow, firstCandidate, reviewSnapshot, semanticReviewChoice )
import Project.Routing
  ( WorkActor, WorkEvent (..), WorkState (..), WorkSource (..)
  , followWork, readWork )
import Project.Types hiding (reviewChecks)
import Project.Work (candidateAtSubmission, decisionContext, projectPrompt)
import Project.WorkPlan

data CoordinatorStart = PlanStarted | PlanAlreadyStarted | PlanUnauthorized
  deriving (Show, Eq)

data QuestionReply
  = QuestionUnauthorized
  | QuestionRefused Text
  | QuestionSent (Either NotificationError NotificationReceipt)
  deriving (Show)

data PlanClose
  = PlanCloseUnauthorized
  | PlanClosePending
  | PlanClosed [Either CheckpointRefusal ()]
  deriving (Show)

data CorrectionReply
  = CorrectionUnauthorized
  | CorrectionRefused Text
  | CorrectionAccepted RequestUpdate
  deriving (Show)

data CorrectionState = CorrectionState
  { correctionRequest :: RequestId
  , correctionChange :: BaselineChange
  , correctionUpdate :: RequestUpdate
  , correctionDelivery :: Maybe (Either ReplyError RequestUpdateState)
  , correctionReported :: Maybe Incorporation
  , correctionReportRefusal :: Maybe Text
  , correctionExpectedChecks :: [Text]
  } deriving (Show)

data ActiveDevelopment = ActiveDevelopment
  { activeResponse :: Response (Outcome Candidate)
  , activeProgress :: Progress WorkProgress
  , activeRouter :: R.ActorHandle (WorkActor (Outcome Candidate))
  , activeTask :: Task
  , activeScopes :: [ComponentScope]
  }

data ActiveReview root where
  ActiveReview
    :: RequestId
    -> R.ActorHandle ReviewFlow
    -> Developed
    -> (Reviewed -> WorkPlan result)
    -> (Either PlanFailure result -> PlanHandler root ())
    -> ActiveReview root

data ActiveVerification root where
  ActiveVerification
    :: Verification value
    -> CheckedSource
    -> Focused.PlanStart
    -> (AcceptedSource value -> WorkPlan result)
    -> (Either PlanFailure result -> PlanHandler root ())
    -> ActiveVerification root

data PlanState root = PlanState
  { planOwner :: AgentRef
  , planInitial :: WorkPlan root
  , planStarted :: Bool
  , planOutcome :: Maybe (Either PlanFailure root)
  , planDevelopments :: [ActiveDevelopment]
  , planReviews :: [ActiveReview root]
  , planHandledDevelopments :: [RequestId]
  , planHandledReviews :: [RequestId]
  , planVerification :: Maybe (ActiveVerification root)
  , planQuestions :: [(RequestId, QuestionReply)]
  , planCompletionRoute :: Maybe (R.Send (Either PlanFailure root))
  , planCompletionAdmission :: Maybe (Either Text ())
  , planJoins :: [SomeJoin root]
  , planCheckpoints :: [ContextCheckpoint]
  , planCheckpointReleases :: Maybe [Either CheckpointRefusal ()]
  , planCorrection :: Maybe CorrectionState
  , planAuxClosed :: Bool
  }

data PlanView root = PlanView
  { viewedOutcome :: Maybe (Either PlanFailure root)
  , viewedDevelopments :: [ActiveDevelopment]
  , viewedReviewCount :: Int
  , viewedQuestionReplies :: [(RequestId, QuestionReply)]
  , viewedCompletionAdmission :: Maybe (Either Text ())
  , viewedReviews :: [R.ActorHandle ReviewFlow]
  , viewedCheckpointReleases :: Maybe [Either CheckpointRefusal ()]
  , viewedCorrection :: Maybe CorrectionState
  }

data DevelopmentStarted root where
  DevelopmentStarted
    :: [ComponentScope] -> Task -> Response (Outcome Candidate) -> Progress WorkProgress
    -> (Developed -> WorkPlan result)
    -> (Either PlanFailure result -> PlanHandler root ())
    -> DevelopmentStarted root

data DevelopmentFinished root where
  DevelopmentFinished
    :: Task -> Response (Outcome Candidate) -> Progress WorkProgress
    -> Either ResponseFailure (ResponseResult (Outcome Candidate))
    -> (Developed -> WorkPlan result)
    -> (Either PlanFailure result -> PlanHandler root ())
    -> DevelopmentFinished root

data PlanResume root where
  PlanResume :: Either PlanFailure (WorkPlan result)
    -> (Either PlanFailure result -> PlanHandler root ())
    -> PlanResume root

data JoinState left right = JoinState
  { joinedLeft :: Maybe (Either PlanFailure left)
  , joinedRight :: Maybe (Either PlanFailure right)
  , joinAdmission :: Maybe (Either Text ())
  }

data Join root left right result mode = Join
  { joinState :: mode :- State (JoinState left right)
  , joinLeft :: mode :- Call (Either PlanFailure left) NoReply
  , joinRight :: mode :- Call (Either PlanFailure right) NoReply
  , joinView :: mode :- Call () (R.Reply (JoinState left right))
  } deriving Generic

type JoinEffects root left right result =
  R.LocalEffects (Join root left right result) '[Actor]

data SomeJoin root where
  SomeJoin :: R.ActorHandle (Join root left right result) -> SomeJoin root

joinDefinition
  :: forall root left right result.
     R.Send (PlanResume root)
  -> ((left, right) -> WorkPlan result)
  -> (Either PlanFailure result -> PlanHandler root ())
  -> ActorSpec (Join root left right result) (JoinEffects root left right result)
joinDefinition destination next complete =
  R.definition "work-plan-join" (Actor.Selected knownEffects) Join
    { joinState = JoinState Nothing Nothing Nothing
    , joinLeft = \answer -> do
        R.modify' (\state -> state { joinedLeft = Just answer })
        publishPair
    , joinRight = \answer -> do
        R.modify' (\state -> state { joinedRight = Just answer })
        publishPair
    , joinView = \() -> R.get
    }
  where
    publishPair = do
      state <- R.get
      case (joinAdmission state, joinedLeft state, joinedRight state) of
        (Nothing, Just left, Just right) -> do
          let resolved = case (left, right) of
                (Right a, Right b) -> Right (next (a, b))
                (Left a, Left b) -> Left (ParallelStopped [a, b])
                (Left a, _) -> Left (ParallelStopped [a])
                (_, Left b) -> Left (ParallelStopped [b])
          admission <- R.trySend destination (PlanResume resolved complete)
          R.modify' (\current -> current { joinAdmission = Just admission })
        _ -> pure ()

data Coordinator root mode = Coordinator
  { coordinatorState :: mode :- State (PlanState root)
  , beginPlan :: mode :- Call () (R.Reply CoordinatorStart)
  , planView :: mode :- Call () (R.Reply (Maybe (PlanView root)))
  , answerQuestion :: mode :- Call (Response (Outcome Candidate), Question, AcceptedDecision) (R.Reply QuestionReply)
  , correctQuestion :: mode :- Call (Response (Outcome Candidate), Question, BaselineChange, [Text]) (R.Reply CorrectionReply)
  , observeCorrection :: mode :- Call () (R.Reply (Maybe CorrectionState))
  , reportIncorporation :: mode :- Call Incorporation NoReply
  , closePlan :: mode :- Call () (R.Reply PlanClose)
  , developmentStarted :: mode :- Call (DevelopmentStarted root) NoReply
  , developmentFinished :: mode :- Call (DevelopmentFinished root) NoReply
  , reviewFinished :: mode :- Call ReviewCompletion NoReply
  , verificationFinished :: mode :- Call CheckState NoReply
  , resumePlan :: mode :- Call (PlanResume root) NoReply
  } deriving Generic

type PlanEffects root = R.LocalEffects (Coordinator root) CodingEffects
type PlanHandler root value = Handler (PlanState root) (PlanEffects root) value

-- | The supplied checkout makes this a coding actor, so it can admit nested
-- coding workers. Parallel branches execute in this same actor; they do not
-- create a second checkout owner.
coordinator
  :: forall effects result. Member Actor effects
  => WorktreeId -> AgentRef -> [ContextCheckpoint] -> WorkPlan result
  -> Maybe (R.Send (Either PlanFailure result))
  -> Eff effects (R.ActorHandle (Coordinator result))
coordinator checkout owner checkpoints plan route = R.start $ R.withWorktree checkout $
  R.definition "work-plan" (Actor.Selected knownEffects) Coordinator
    { coordinatorState = PlanState owner plan False Nothing [] [] [] [] Nothing [] route Nothing [] checkpoints Nothing Nothing False
    , beginPlan = \() -> do
        state <- R.get
        authorized <- isOwner @result state
        if not authorized then pure PlanUnauthorized
        else if planStarted state then pure PlanAlreadyStarted
        else do
          R.modify' (\current -> current { planStarted = True })
          own <- R.self @(Coordinator result)
          drive own (planInitial state) (finishPlan own)
          pure PlanStarted
    , planView = \() -> do
        state <- R.get
        authorized <- isOwner @result state
        pure $ if authorized then Just PlanView
          { viewedOutcome = planOutcome state
          , viewedDevelopments = planDevelopments state
          , viewedReviewCount = length (planReviews state)
          , viewedQuestionReplies = planQuestions state
          , viewedCompletionAdmission = planCompletionAdmission state
          , viewedReviews = [flow | ActiveReview _ flow _ _ _ <- planReviews state]
          , viewedCheckpointReleases = planCheckpointReleases state
          , viewedCorrection = planCorrection state
          } else Nothing
    , closePlan = \() -> do
        state <- R.get
        authorized <- isOwner @result state
        if not authorized then pure PlanCloseUnauthorized
        else case planOutcome state of
          Nothing -> pure PlanClosePending
          Just _ -> case planCheckpointReleases state of
            Just released -> do
              if planAuxClosed state then pure () else do
                mapM_ (void . R.finish . activeRouter) (planDevelopments state)
                mapM_ (\(SomeJoin join) -> void (R.finish join)) (planJoins state)
                R.modify' (\current -> current { planAuxClosed = True })
              pure (PlanClosed released)
            Nothing -> pure PlanClosePending
    , answerQuestion = \(response, question, decision) -> do
        state <- R.get
        authorized <- isOwner @result state
        if not authorized then pure QuestionUnauthorized else do
          let matching = [active | active <- planDevelopments state,
                requestId (activeResponse active) == requestId response]
          case matching of
            [active] -> do
              work <- readWork (activeRouter active)
              let current = [item | source <- collectedWork work,
                    item <- workQuestions (sourceProgress source)]
              observed <- pollResponse response
              if question `notElem` current then pure (QuestionRefused "question is not current")
              else if decisionQuestion decision /= question then pure (QuestionRefused "answer names another question")
              else if decisionSource decision /= taskSource (activeTask active) then
                pure (QuestionRefused "answer source differs from the active task")
              else case observed of
                ResponsePending _ -> deliverDecision response decision
                ResponseStarting _ -> deliverDecision response decision
                _ -> pure (QuestionRefused "development request settled")
            _ -> pure (QuestionRefused "request is not an active development")
    , correctQuestion = \(response, question, change, checks) -> do
        state <- R.get
        authorized <- isOwner @result state
        if not authorized then pure CorrectionUnauthorized
        else if maybe False (const True) (planCorrection state) then
          pure (CorrectionRefused "one correction episode is already recorded")
        else case [active | active <- planDevelopments state,
          requestId (activeResponse active) == requestId response] of
          [active] -> do
            work <- readWork (activeRouter active)
            let current = [item | source <- collectedWork work,
                  item <- workQuestions (sourceProgress source)]
                valid = do
                  if question `elem` current then Right ()
                    else Left "question is not current"
                  validateBaselineFor change (activeTask active) question checks
                  mapM_ (\scope -> case checkComponentAmendment scope
                    (baselineAmendment change) of
                      Left failure -> Left (Text.pack (show failure))
                      Right () -> Right ()) (activeScopes active)
                admitCorrection = do
                  let target = Affected "coordinator" (activeResponse active)
                        (responseActor (activeResponse active)) (activeTask active) question checks
                  updated <- updateRequest response (incorporationUpdate change target)
                  case updated of
                    Left refused -> pure (CorrectionRefused (Text.pack (show refused)))
                    Right update -> do
                      R.modify' (\current -> current { planCorrection = Just
                        (CorrectionState (requestId response) change update Nothing Nothing Nothing checks) })
                      pure (CorrectionAccepted update)
            observed <- pollResponse response
            case (valid, observed) of
              (Left reason, _) -> pure (CorrectionRefused reason)
              (_, ResponsePending _) -> admitCorrection
              (_, ResponseStarting _) -> admitCorrection
              _ -> pure (CorrectionRefused "development request settled")
          _ -> pure (CorrectionRefused "request is not an active development")
    , observeCorrection = \() -> do
        state <- R.get
        authorized <- isOwner @result state
        if not authorized then pure Nothing
        else case planCorrection state of
          Nothing -> pure Nothing
          Just correction -> do
            observed <- pollRequestUpdate (correctionUpdate correction)
            let refreshed = correction { correctionDelivery = Just observed }
            R.modify' (\current -> current { planCorrection = Just refreshed })
            pure (Just refreshed)
    , reportIncorporation = \report -> do
        state <- R.get
        origin <- R.sender @(Coordinator result)
        case planCorrection state of
          Nothing -> pure ()
          Just correction -> do
            let matching = [active | active <- planDevelopments state,
                  requestId (activeResponse active) == correctionRequest correction]
            case matching of
              [active] | correctionRequest correction `notElem` planHandledDevelopments state
                && origin == ActorMessageFrom
                (agentIdentity (responseActor (activeResponse active))) ->
                  case report of
                    Incorporated amendment source checks
                      | amendment == baselineAmendment (correctionChange correction)
                        && source == baselineAfter (correctionChange correction)
                        && all (`elem` checks) (correctionExpectedChecks correction) -> do
                          R.modify' (\current -> current { planCorrection = Just
                            (correction { correctionReported = Just report }) })
                          pure ()
                    IncorporationBlocked amendment _ _
                      | amendment == baselineAmendment (correctionChange correction) -> do
                          R.modify' (\current -> current { planCorrection = Just
                            (correction { correctionReported = Just report
                              , correctionReportRefusal = Just "worker reported blocked incorporation" }) })
                          pure ()
                    _ -> R.modify' (\current -> current { planCorrection = Just
                      (correction { correctionReportRefusal = Just
                        "incorporation names another amendment, baseline, or checks" }) })
              _ -> pure ()
    , developmentStarted = \(DevelopmentStarted scopes task response progress next complete) -> do
        own <- R.self @(Coordinator result)
        router <- followWork [("worker", response, progress)] $ \event -> case event of
          WorkFinished _ receipt -> do
            accepted <- R.trySend (developmentFinished own)
              (DevelopmentFinished task response progress receipt next complete)
            case accepted of
              Left reason -> Just <$> sendMessage owner
                ("work-plan result route refused for " <> Text.pack (show (requestId response)) <> ": " <> reason)
              Right () -> pure Nothing
          _ -> pure Nothing
        R.modify' (\state -> state { planDevelopments = planDevelopments state
          ++ [ActiveDevelopment response progress router task scopes] })
    , developmentFinished = \(DevelopmentFinished task response progress result next complete) -> do
        active <- R.gets planDevelopments
        handled <- R.gets planHandledDevelopments
        if requestId response `notElem` handled
          && any ((== requestId response) . requestId . activeResponse) active then do
          R.modify' (\state -> state
            { planHandledDevelopments = planHandledDevelopments state ++ [requestId response] })
          let developed = case result of
                Left failure -> Left (AdmissionRefused DevelopmentNode (Text.pack (show failure)))
                Right receipt
                  | executionRequest (responseExecution receipt) /= requestId response ->
                      Left (SourceRefused DevelopmentNode "response request identity differs")
                  | Blocked reason evidence <- responseValue receipt -> Left (WorkerBlocked reason evidence)
                  | Produced candidate <- responseValue receipt -> case candidateAtSubmission candidate (responseWorktree receipt) of
                      Left reason -> Left (SourceRefused DevelopmentNode reason)
                      Right exact -> Right (Developed task exact response progress receipt)
          settled <- R.gets planOutcome
          case settled of
            Just _ -> releaseIfQuiescent
            Nothing -> case developed of
              Left failure -> complete (Left failure)
              Right value -> do
                own <- R.self @(Coordinator result)
                drive own (next value) complete
        else pure ()
    , reviewFinished = \completion -> do
        own <- R.self @(Coordinator result)
        continueReview own completion
    , verificationFinished = \checks -> do
        own <- R.self @(Coordinator result)
        continueVerification own checks
    , resumePlan = \(PlanResume resumed complete) -> do
        settled <- R.gets planOutcome
        case settled of
          Just _ -> releaseIfQuiescent
          Nothing -> case resumed of
            Left failure -> complete (Left failure)
            Right plan -> do
              own <- R.self @(Coordinator result)
              drive own plan complete
    }
  where
    deliverDecision response decision = do
      sent <- sendMessage (responseActor response) (decisionContext decision)
      R.modify' (\state -> state { planQuestions = planQuestions state
        ++ [(requestId response, QuestionSent sent)] })
      pure (QuestionSent sent)

isOwner :: forall root. PlanState root -> PlanHandler root Bool
isOwner state = do
  origin <- R.sender @(Coordinator root)
  pure $ origin == ActorMessageFrom (agentIdentity (planOwner state))

finishPlan :: Coordinator root R.Self -> Either PlanFailure root -> PlanHandler root ()
finishPlan _ result = do
  prior <- R.gets planOutcome
  case prior of
    Just _ -> pure ()
    Nothing -> do
      R.modify' (\state -> state { planOutcome = Just result })
      releaseIfQuiescent
      route <- R.gets planCompletionRoute
      case route of
        Nothing -> pure ()
        Just destination -> do
          admission <- R.trySend destination result
          R.modify' (\state -> state { planCompletionAdmission = Just admission })

releaseIfQuiescent :: PlanHandler root ()
releaseIfQuiescent = do
  state <- R.get
  let reviewsSettled = all (\(ActiveReview request _ _ _ _) ->
        request `elem` planHandledReviews state) (planReviews state)
      verificationSettled = case planVerification state of
        Nothing -> True
        Just _ -> False
  case (planOutcome state, planCheckpointReleases state) of
    (Just _, Nothing)
      | all (\active -> requestId (activeResponse active)
          `elem` planHandledDevelopments state) (planDevelopments state)
          && reviewsSettled
          && verificationSettled -> do
          released <- mapM releaseCheckpoint (planCheckpoints state)
          R.modify' (\current -> current { planCheckpointReleases = Just released })
    _ -> pure ()

drive
  :: forall root value. Coordinator root R.Self -> WorkPlan value
  -> (Either PlanFailure value -> PlanHandler root ())
  -> PlanHandler root ()
drive own plan complete = case stepPlan plan of
  Finished value -> complete (Right value)
  NeedDevelopment scopes input next -> case checkDevelopment scopes input of
    Left failure -> complete (Left failure)
    Right task -> case input of
      RetainedWorker worker label _ -> do
        _ <- requestWithProgressInto @WorkProgress @(Outcome Candidate) worker
          ((assignment label task) { guidance = Just (projectPrompt "task"), report = Silent })
          (\(response, progress) ->
            R.send (developmentStarted own)
              (DevelopmentStarted scopes task response progress next complete))
        pure ()
      ForkWorker _ makeBranch -> do
        attempted <- attemptUnfold (taskGroup task) $
          childWithProgress @WorkProgress @(Outcome Candidate)
            (makeBranch (WorkerAssignment task (reportIncorporation own)))
        case attempted of
          Left reason -> complete (Left (AdmissionRefused DevelopmentNode (Text.pack (show reason))))
          Right (response, progress) ->
            R.send (developmentStarted own)
              (DevelopmentStarted scopes task response progress next complete)
  NeedReview scopes spec developed next -> case checkReviewScope scopes spec of
    Left failure -> complete (Left failure)
    Right policy -> do
      owner <- R.gets planOwner
      let policy' = policy { flowCompleted = Just (reviewFinished own) }
      flow <- R.start (R.withWorktree (reviewCheckout spec)
        (checkedReviewFlow owner (reviewTask spec) policy'
          (developedRequest developed) (reviewChecks spec) semanticReviewChoice))
      route <- R.forwardResult (developedRequest developed)
        (firstCandidate (R.client flow))
      R.modify' (\state -> state { planReviews = planReviews state ++
        [ActiveReview (requestId (developedRequest developed)) flow developed next complete] })
      route `seq` pure ()
  NeedIntegration _ spec reviewed next -> do
    let source = responseWorktree (developedReceipt (reviewedDevelopment reviewed))
    case source of
      WorktreeObserved receipt _ _ -> do
        let candidate = checkpointCandidate (reviewedProof reviewed)
            target = Merge.mergeActor (integrationTarget spec)
            request = Merge.PublishRequest
              (integrationTaskName spec) (treeId receipt)
              (candidateCommit candidate) (integrationMessage spec)
        published <- R.call (Merge.publish (R.client target)) request
        case published of
          Merge.Published headOid previous _ ->
            drive own (next (CheckedSource reviewed headOid previous published)) complete
          other -> complete (Left (IntegrationStopped other))
      _ -> complete (Left (SourceRefused IntegrationNode "candidate worktree evidence is unavailable"))
  NeedVerification _ spec checked next -> do
    if null (verificationChecks spec) then
      complete (Left (VerificationStopped "no product checks declared"))
    else do
      started <- startCheckPlanInto (verificationFinished own)
        (checkedHead checked) (verificationChecks spec)
      case started of
        Left reason -> complete (Left (VerificationStopped (Text.pack (show reason))))
        Right planStart -> R.modify' (\state -> state
          { planVerification = Just (ActiveVerification spec checked planStart next complete) })
  NeedParallel _ left right next -> do
    join <- R.start (joinDefinition (resumePlan own) next complete)
    R.modify' (\state -> state { planJoins = planJoins state ++ [SomeJoin join] })
    drive own left $ \answer -> do
      admission <- R.trySend (joinLeft (R.client join)) answer
      case admission of
        Right () -> pure ()
        Left reason -> complete (Left (AdmissionRefused DevelopmentNode
          ("parallel left join refused: " <> reason)))
    drive own right $ \answer -> do
      admission <- R.trySend (joinRight (R.client join)) answer
      case admission of
        Right () -> pure ()
        Left reason -> complete (Left (AdmissionRefused DevelopmentNode
          ("parallel right join refused: " <> reason)))

checkDevelopment :: [ComponentScope] -> Development -> Either PlanFailure Task
checkDevelopment scopes input = do
  let task = case input of
        RetainedWorker _ _ current -> current
        ForkWorker current _ -> current
  mapM_ (`checkComponentTask` task) scopes
  pure task

checkReviewScope :: [ComponentScope] -> ReviewSpec -> Either PlanFailure ReviewFlowPolicy
checkReviewScope scopes spec = do
  mapM_ (`checkComponentTask` reviewTask spec) scopes
  limit <- effectiveRepairLimit scopes
  pure ((reviewPolicy spec) { flowRepairLimit = min limit (flowRepairLimit (reviewPolicy spec)) })

continueReview :: Coordinator root R.Self -> ReviewCompletion -> PlanHandler root ()
continueReview own completion = do
  reviews <- R.gets planReviews
  handled <- R.gets planHandledReviews
  case completion of
    ReviewRefused request reason | request `notElem` handled -> case select request reviews of
      Just (ActiveReview _ _ _ _ complete) -> do
        mark request
        settled <- R.gets planOutcome
        case settled of
          Just _ -> releaseIfQuiescent
          Nothing -> complete (Left (ReviewStopped (Text.pack (show reason))))
      Nothing -> pure ()
    ReviewApproved request proof | request `notElem` handled -> case select request reviews of
      Just (ActiveReview _ flow developed next complete) -> do
        mark request
        settled <- R.gets planOutcome
        case settled of
          Just _ -> releaseIfQuiescent
          Nothing -> do
            state <- R.call (reviewSnapshot (R.client flow)) ()
            drive own (next (Reviewed developed proof state)) complete
      Nothing -> pure ()
    _ -> pure ()
  where
    mark request = R.modify' (\state -> state
      { planHandledReviews = planHandledReviews state ++ [request] })
    select request = first . filter matches
      where matches (ActiveReview current _ _ _ _) = current == request
    first (value : _) = Just value
    first [] = Nothing

continueVerification :: Coordinator root R.Self -> CheckState -> PlanHandler root ()
continueVerification own checks = do
  active <- R.gets planVerification
  case active of
    Nothing -> pure ()
    Just (ActiveVerification spec checked started next complete) -> do
      let report = PlanReport started (Just checks)
      R.modify' (\state -> state { planVerification = Nothing })
      settled <- R.gets planOutcome
      case Focused.planWatcher started of
        Just (Right watcher) -> do
          _ <- finishChecks watcher
          pure ()
        _ -> pure ()
      if maybe False (const True) settled then releaseIfQuiescent
      else if not (planPassed report) then
        complete (Left (VerificationStopped (planSummary report)))
      else case verificationAccept spec checked report of
        Left reason -> complete (Left (VerificationStopped reason))
        Right value -> drive own
          (next (AcceptedSource value checked report)) complete
