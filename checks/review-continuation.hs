import GHC.Generics (Generic)
import qualified Tidepool.Actor as Actor
type ReviewWave = ActorHandle (WorkActor (Outcome ReviewDecision))
data RetainedReviewState = RetainedReviewState { reviewCollectors :: [(Response (Outcome ReviewDecision), ReviewWave)], reviewEvents :: [WorkEvent (Outcome ReviewDecision)], completedReviews :: [WorkState (Outcome ReviewDecision)], stoppedCandidates :: [Settlement (Outcome Candidate)], stoppedNotices :: [Either NotificationError NotificationReceipt], repairAttempts :: [(Response (Outcome Candidate), Forwarding (Outcome Candidate))], candidateReceipts :: [Either ResponseFailure (ResponseResult (Outcome Candidate))], sourceProblems :: [(ExecutionReceipt, Text)] }
instance Show RetainedReviewState where show state = "RetainedReviewState " ++ show (length (reviewCollectors state), length (completedReviews state), length (reviewEvents state), length (stoppedCandidates state), length (stoppedNotices state), length (repairAttempts state), length (sourceProblems state))
data RetainedReviewFixture mode = RetainedReviewFixture { reviewState :: mode :- State RetainedReviewState, reviewStarted :: mode :- Call (Response (Outcome ReviewDecision), Progress WorkProgress) NoReply, reviewEvent :: mode :- Call (Response (Outcome ReviewDecision), WorkEvent (Outcome ReviewDecision)) NoReply, reviewView :: mode :- Call () (R.Reply RetainedReviewState), repairStarted :: mode :- Call (Response (Outcome Candidate), Progress WorkProgress) NoReply, repairDone :: mode :- Call (Either ResponseFailure (ResponseResult (Outcome Candidate))) NoReply, candidateDone :: mode :- Event (Either ResponseFailure (ResponseResult (Outcome Candidate))) } deriving Generic
let startReview = (\own result -> do
        modify' (\state -> state { candidateReceipts = candidateReceipts state ++ [result] })
        case result of
          Right receipt | Produced candidate <- responseValue receipt -> case candidateAtSubmission candidate (responseWorktree receipt) of
            Right selected -> do
              _ <- requestWithProgressInto @WorkProgress @(Outcome ReviewDecision)
                (responseActor reviewer)
                ((assignment reviewLabel (ReviewRequest (AssignedTask task) selected repairPolicy)) { guidance = Just (projectPrompt "review"), report = Silent }) $ R.send (reviewStarted (own :: RetainedReviewFixture Self))
              pure ()
            Left reason -> do
              modify' (\state -> state { sourceProblems = sourceProblems state ++ [(responseExecution receipt, reason)] })
              sent <- sendMessage owner reason
              modify' (\state -> state { stoppedNotices = stoppedNotices state ++ [sent] })
          _ -> do
            let settled = either ReplyUnavailable ReplyAvailable result
            modify' (\state -> state { stoppedCandidates = stoppedCandidates state ++ [settled] })
            case onStopped settled of
              Nothing -> pure ()
              Just message -> do
                sent <- sendMessage owner message
                modify' (\state -> state { stoppedNotices = stoppedNotices state ++ [sent] })
      ) :: RetainedReviewFixture Self -> Either ResponseFailure (ResponseResult (Outcome Candidate)) -> Handler RetainedReviewState (CoordinationEffects RetainedReviewFixture) ()
let reviewBoxDefinition = coordinationActor "review-handoff" RetainedReviewFixture
      { reviewState = RetainedReviewState { reviewCollectors = [], reviewEvents = [], completedReviews = [], stoppedCandidates = [], stoppedNotices = [], repairAttempts = [], candidateReceipts = [], sourceProblems = [] }
      , reviewView = \() -> get
      , reviewStarted = \(attempt, updates) -> do
          own <- R.self @RetainedReviewFixture
          collector <- followWork [("review", attempt, updates)] (WorkSink $ \event -> do { R.send (reviewEvent own) (attempt, event); runWorkSink onReview event })
          modify' (\state -> state { reviewCollectors = reviewCollectors state ++ [(attempt, collector)] })
      , reviewEvent = \(attempt, event) -> do
          modify' (\state -> state { reviewEvents = reviewEvents state ++ [event] })
          case event of
            WorkFinished _ result -> do
              active <- gets reviewCollectors
              case [collector | (response, collector) <- active, requestId response == requestId attempt] of
                [collector] -> do
                  completed <- finishWork collector
                  modify' (\state -> state { reviewCollectors = [(response, retained) | (response, retained) <- reviewCollectors state, requestId response /= requestId attempt], completedReviews = case completed of { Actor.Completed value -> completedReviews state ++ [value]; _ -> completedReviews state } })
                _ -> error "review result has no unique owned collector"
              case (repairPolicy, result) of
                (RetainedImplementer implementer, Right receipt) | Produced (Repair candidate findings) <- responseValue receipt -> do
                  own <- R.self @RetainedReviewFixture
                  _ <- requestWithProgressInto @WorkProgress @(Outcome Candidate) implementer
                    ((assignment repairLabel (RepairTask task candidate findings)) { guidance = Just (projectPrompt "repair"), report = Silent })
                    (R.send (repairStarted own))
                  pure ()
                _ -> pure ()
            _ -> pure ()
      , repairStarted = \(attempt, _) -> do
          own <- R.self @RetainedReviewFixture
          forwarding <- R.forwardResult attempt (repairDone own)
          modify' (\state -> state { repairAttempts = repairAttempts state ++ [(attempt, forwarding)] })
      , repairDone = \result -> do
          own <- R.self @RetainedReviewFixture
          startReview own result
      , candidateDone = R.on (R.settlement worker) $ \result -> do
          own <- R.self @RetainedReviewFixture
          startReview own result
      }
reviewBox <- R.start reviewBoxDefinition
