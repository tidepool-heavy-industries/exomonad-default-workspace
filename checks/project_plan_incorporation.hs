let AssignedTask assignedTask = reviewBasis sessionInput
let Right (Right (AmendPlan amendment)) = design
let RetainedImplementer implementer = repairOwner sessionInput
Right planResponse <- requestIncorporation implementer "incorporate-plan" assignedTask amendment
planReady <- await (settlement planResponse)
