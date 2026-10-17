import qualified Examples.ModelCallWorkflow as ModelExample
-- No ambient conversation, source or tools are included. Put the actual bounded
-- evidence in this input; a path alone does not make its contents available.
let input = "Intent: decide whether to investigate the retry loop. Evidence: one retained test failed after three retries; no implementation or timing trace is supplied." :: Text
result <- ModelExample.synchronousFlash input
display (ModelExample.report result)
-- Pro variant: use ModelExample.synchronousPro input in this notebook.
-- Ordinary asynchronous programs can use ModelExample.flash or ModelExample.pro.
-- This is a fresh invocation when run: it does not reuse the first invocation's response.
result
