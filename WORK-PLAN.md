# Authored work plans

`Project.WorkPlan` builds a typed plan with ordinary `do`, `parallel`, and
`Component`. `develop` returns a `Developed` value containing the original
request, progress, receipt, and checked candidate. `review` consumes that exact
value; `integrate` consumes its review proof; `verify` consumes the checked
publication. The coordinator interprets nodes as resident actor work and keeps
the original response handles. `Project.WorkPlanChecks.structural` is the
compiled starting example for composition and the typed parallel join.

Start `Project.WorkPlanCoordinator.coordinator` with an unbound managed checkout,
the owner, checkpoint tokens whose custody this plan takes, the plan, and an
optional terminal route. The owner calls `beginPlan` once and observes
`planView`. New Luna workers can use `ForkWorker task (lunaWorker label Medium
source)`; `sessionInput :: WorkerAssignment` contains both the Task and the
typed `incorporationRoute`. A branch that needs inherited context can build its
own branch with `withContext (fromCheckpoint seed)` and pass `seed` to the
coordinator's custody list. The runtime fences cross-session seeds.

`answerQuestion` sends an ordinary decision message to the exact pending
worker. Use `correctQuestion` only when an accepted baseline changes that
request's obligation. It validates the current full question, task source,
amendment, decision, and component scope, and calls `updateRequest` on the
original response once. `observeCorrection` records presentation separately;
the worker sends `Incorporation` on its typed route while its request stays
pending. Reported checks are worker evidence, not executed verification. A
refused or uncertain update remains recorded and never starts replacement
work.

After terminal outcome, `closePlan` releases owned checkpoint tokens and
finishes routing and join actors. It returns `PlanClosePending` while work is
still active. `viewedReviews` retains exact ReviewFlow handles: the original
owner calls `reviewCleanup` on each terminal flow, inspects its cleanup
receipts, and then finishes that flow. The coordinator cannot impersonate that
owner. Preexisting retained workers need a reporting capability assigned
before their session started; a text message cannot add one to `sessionInput`.
