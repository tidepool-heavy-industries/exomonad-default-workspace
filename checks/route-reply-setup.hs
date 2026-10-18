{-# LANGUAGE QuasiQuotes #-}
import qualified Project.RecursiveSpec as Recursive
import Tidepool.Model (modelPermissions, allowSpawnAgent)
let work = (task "delivery-lead" "Deliver the feature" ["feature.txt"] "Retain the exact evidence" sourceHead)
      { planPath = "plans/current/feature.md", rationale = "Retain request ownership through automatic forwarding." }
Right leadAgent <- spawnSubagent (FreshCtx (taskContext work)) (ForkWorktree projectHead)
  ((defaultSpawnOptions currentModel (Recursive.recursiveSpec currentModel))
    { spawnPermissions = modelPermissions [allowSpawnAgent currentModel]
    , spawnInstructions = Just (projectPrompt "task"), spawnLabel = Just (taskName work) })
Right lead <- request @Candidate leadAgent work defaultRequestOptions
