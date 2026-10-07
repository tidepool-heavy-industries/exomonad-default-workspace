let AssignedTask assignedTask = reviewBasis sessionInput
let designLabel = "boundary-question" :: Text
let designWatch = "design-answer" :: WatchLabel
let slot = DesignSlot "plans/current/architecture.md" designLabel designWatch "planner" Medium
let WatchReady repairedResult = state
let Right (Produced repairedCandidate) = settledValue repairedResult
let question = (designQuestion assignedTask repairedCandidate "Does preparation preserve the boundary?") { questionAlternatives = ["retain the gate", "expand acceptance"], questionUnblocks = ["feature review"] }
(expert, designReady) <- consultDesign slot question
