import qualified Examples.ModelCallWorkflow as ModelExample
import qualified Exomonad.Models as Models
-- No ambient conversation, source or tools are included. Put the actual bounded
-- evidence in this input; a path alone does not make its contents available.
let input = "Intent: decide whether to investigate the retry loop. Evidence: one retained test failed after three retries; no implementation or timing trace is supplied." :: Text
-- Each call makes a fresh invocation.
result <- ModelExample.synchronousWithModel Models.primaryModel input
display (ModelExample.report result)
-- Choose another admitted entry from Models.allowedModels to change the model.
-- Ordinary asynchronous programs use ModelExample.invokeWithModel choice input.
result
