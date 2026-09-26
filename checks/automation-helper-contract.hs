{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless, void)
import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import Project.AssumptionWatch (watchIncorporatedBaseline)
import Project.ParallelInvestigate
import Project.Types (Incorporation)
import Tidepool.Actors.Exomonad (Actor, AgentRef, GitOid, Response)
import qualified Tidepool.Command as Cmd
import Tidepool.Effects.Core (Commands)

-- A parent can retain the pending child's baseline while starting two exact,
-- read-only command probes in its own checkout. The returned launch retains
-- any jobs that need later observation and the probes not yet run.
pendingChildExample
  :: (Member Actor effects, Member Commands effects)
  => AgentRef -> GitOid -> Response Incorporation -> Text
  -> Eff effects (Either ProbeRefusal (ProbeLaunch, [ProbeObservation]))
pendingChildExample owner baseline incorporation checkout = do
  void (watchIncorporatedBaseline owner baseline "pending child" incorporation)
  result <- startProbeBatch (ProbeLimits 2 2)
    [ CommandProbe "status" "working tree status" checkout (Cmd.MiB 128)
        (Cmd.argv ["git", "status", "--short"])
    , CommandProbe "head" "current commit" checkout (Cmd.MiB 128)
        (Cmd.argv ["git", "rev-parse", "HEAD"])
    ] ["status", "head"]
  case result of
    Left refusal -> pure (Left refusal)
    Right launch -> do
      observations <- observeProbeBatch 0 launch
      pure (Right (launch, observations))

assert :: String -> Bool -> IO ()
assert label passed = unless passed (error label)

main :: IO ()
main = do
  let probe name = CommandProbe name name "/tmp" (Cmd.MiB 64) (Cmd.argv ["true"])
      available = map probe ["one", "two", "three"]
      names plan = map probeName (plannedStart plan)
  assert "refuse duplicate available names"
    (case selectProbes [probe "one", probe "one"] ["one"] of
      Left (DuplicateAvailableProbe "one") -> True
      _ -> False)
  assert "refuse duplicate requested names"
    (case selectProbes available ["one", "one"] of
      Left (DuplicateRequestedProbe "one") -> True
      _ -> False)
  assert "refuse unknown requested names"
    (case selectProbes available ["missing"] of
      Left (UnknownRequestedProbe "missing") -> True
      _ -> False)
  assert "cap active and total selected probes separately"
    (case planProbeBatch (ProbeLimits 2 1) available ["one", "two", "three"] of
      Right plan -> names plan == ["one"] && plannedUnrun plan == ["two"]
        && plannedOutsideBudget plan == ["three"]
      _ -> False)
  assert "refuse implicit working directory"
    (case planProbeBatch (ProbeLimits 1 1) [(probe "one") { probeDirectory = "relative" }] ["one"] of
      Left (InvalidProbeDirectory "one") -> True
      _ -> False)
  assert "refuse invalid memory before starting"
    (case planProbeBatch (ProbeLimits 1 1) [(probe "one") { probeMemory = Cmd.MiB 0 }] ["one"] of
      Left (InvalidProbeMemory "one") -> True
      _ -> False)
