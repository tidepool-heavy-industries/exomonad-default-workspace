initialState <- pollResponse worker
let ResponseReady initialAnswer = initialState
let Produced initialCandidate = responseValue initialAnswer
(reviewer, reviewQuestions) <- reviewCandidate currentModel task (RetainedImplementer (responseActor worker)) initialCandidate
