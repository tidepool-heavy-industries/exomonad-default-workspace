let AssignedTask assignedTask = reviewBasis sessionInput
let WatchReady designResult = design
let Right (AmendPlan amendment) = settledValue designResult
let RetainedImplementer implementer = repairOwner sessionInput
Right planResponse <- requestIncorporation implementer "incorporate-plan" assignedTask amendment
let planWatch = "plan-incorporated" :: WatchLabel
planRetention <- detachRequest planResponse
planReady <- case planRetention of
  Right () -> watch planWatch (awaitSettled planResponse)
  Left issue -> error (T.pack (show issue))
