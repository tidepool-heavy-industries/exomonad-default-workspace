{-# LANGUAGE QuasiQuotes #-}
import Tidepool.Effects.Core (GitRef (..))
-- Bind chooseReviewRoute = semanticReviewChoice for a live semantic decision,
-- or supply a bounded policy callback. Only exact-source Repair reaches it.
let campaign = campaignName :: CampaignLabel
let task = Task (batch campaign "component") "plans/component.md" sourceHead
      "Implement one component" "Review its exact committed candidate"
      ["review-flow.txt"] "Read exact committed source" []
(worker, _updates) <- unfold (taskGroup task)
  (childWithProgress @WorkProgress @(Outcome Candidate)
    (coding (atRef (GitRef (renderGitOid sourceHead))) (assignment [label|implement|] task)))
Right coordinatorTree <- createWorktree
  (fromRef (GitRef (renderGitOid sourceHead)) coordinatorName)
let policy = defaultReviewFlowPolicy
      { flowRepairLimit = 1
      , flowEscalationCriteria = ["A finding changes the assigned paths or acceptance."] }
flow <- R.start (R.withWorktree (worktreeId coordinatorTree)
  (reviewFlowWith me task policy worker chooseReviewRoute))
initialRoute <- R.forwardResult worker (firstCandidate (R.client flow))
initialSnapshot <- R.call (reviewSnapshot (R.client flow)) ()
pendingCleanup <- R.call (reviewCleanup (R.client flow)) ReviewCleanupOnce
inspectFull (show (flowStage initialSnapshot, pendingCleanup))
