import qualified Examples.ModelCallWorkflow as ModelExample
-- No ambient conversation, source or tools are included. Put the actual bounded
-- evidence in this input; a path alone does not make its contents available.
let input = "Intent: decide whether to investigate the retry loop. Evidence: one retained test failed after three retries; no implementation or timing trace is supplied." :: Text
-- Each call makes a fresh invocation.
result <- ModelExample.synchronousWithModel currentModel input
display (ModelExample.report result)
-- Pass another typed Model family only when its model-call permission is granted.
-- Ordinary asynchronous programs use ModelExample.invokeWithModel model input.
result
