{-# LANGUAGE QuasiQuotes #-}
import qualified Tidepool.Actor.Record as R
import qualified Project.RecursiveSpec as Recursive
import Tidepool.Model (modelPermissions, allowForkAgent, allowSpawnAgent)
let leftTask = task "left" "Investigate the left boundary" ["src/left.rs"] "Return findings with references; no code candidate required" sourceHead
let rightTask = task "right" "Investigate the right boundary" ["src/right.rs"] "Return findings with references; no code candidate required" sourceHead
-- Descendants use the explicit retained prefix; the root receives task guidance.
Right localCheckpoint <- Tidepool.Actors.Exomonad.checkpoint "recursive-requests"
let childOptions work = (defaultSpawnOptions currentModel (Recursive.recursiveSpec currentModel))
      { spawnPermissions = modelPermissions [allowForkAgent currentModel, allowSpawnAgent currentModel]
      , spawnInstructions = Just (projectPrompt "task"), spawnLabel = Just (taskName work) }
let spawnChild work = if phaseName == "first"
      then spawnSubagent (FreshCtx (taskContext work)) (ForkWorktree currentCheckout) (childOptions work)
      else spawnSubagent (ForkCtx localCheckpoint) (ForkWorktree currentCheckout) (childOptions work)
let sink :: WorkSink (Outcome Text)
    sink = notifyWork me (workMessage (\result -> T.pack (show (result :: Outcome Text))))
Right leftAgent <- spawnChild leftTask
Right (leftRequest, leftProgress) <- requestWithProgress @WorkProgress @(Outcome Text) leftAgent leftTask
  (defaultRequestOptions { requestReporting = Silent })
Right rightAgent <- spawnChild rightTask
Right (rightRequest, rightProgress) <- requestWithProgress @WorkProgress @(Outcome Text) rightAgent rightTask
  (defaultRequestOptions { requestReporting = Silent })
(collection, answerer) <- if phaseName == "subcomponents" then do
  Right (answerSpec, workForAnswers) <- pure (answeredWorkDefinition me
    [("left", leftTask, leftAgent, R.settlement leftRequest, R.progress leftProgress),
     ("right", rightTask, rightAgent, R.settlement rightRequest, R.progress rightProgress)] sink)
  answers <- R.start answerSpec
  admitted <- R.start (workForAnswers answers)
  pure (admitted, Just answers)
  else do
    Right admitted <- traverse R.start (workDefinition [("left", R.settlement leftRequest, R.progress leftProgress), ("right", R.settlement rightRequest, R.progress rightProgress)] sink)
    pure (admitted, Nothing)
releaseCheckpoint localCheckpoint
