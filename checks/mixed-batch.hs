Right textAgent <- spawnSubagent (FreshCtx "Inspect the text source.") (ForkWorktree projectHead)
  ((defaultSpawnOptions workspaceAgentSpec) { spawnInstructions = Just (projectPrompt "task"), spawnLabel = Just "text-worker" })
Right (textRequest, textProgress) <- requestWithProgress @WorkProgress @Text textAgent "Inspect text" defaultRequestOptions
Right numberAgent <- spawnSubagent (FreshCtx "Inspect the number source.") (ForkWorktree projectHead)
  ((defaultSpawnOptions workspaceAgentSpec) { spawnInstructions = Just (projectPrompt "task"), spawnLabel = Just "number-worker" })
Right (numberRequest, numberProgress) <- requestWithProgress @WorkProgress @Text numberAgent "Inspect numbers" defaultRequestOptions
Right collection <- followWork [("text", textRequest, textProgress), ("number", numberRequest, numberProgress)] keepWork
