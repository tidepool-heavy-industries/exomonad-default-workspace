{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Focused resident checks of actual command-job and record-actor custody.
module Project.AutomationRuntimeChecks (commandCustody) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import qualified Data.Text as Text
import Project.Checks (script)
import Tidepool.Check

commandCustody :: Member RecipeCheck effects => Eff effects ()
commandCustody = do
  owner <- root
  script owner "automation-command-setup"
  started <- turn owner "inspectFull launched"
  check "both supplied command probes start with exact job handles"
    (all (`Text.isInfixOf` output started) ["ProbeRunning \"first\"", "ProbeRunning \"second\""])
  first <- turn owner "let Right batch = launched\nfirstObserved <- observeProbe 1000 (head (startedProbes batch))\ninspectFull firstObserved"
  check "first retained probe yields its own stdout page"
    ("ProbeObserved \"first\"" `Text.isInfixOf` output first
      && "first" `Text.isInfixOf` output first)
  second <- turn owner "let Right batch = launched\nsecondObserved <- observeProbe 1000 (startedProbes batch !! 1)\ninspectFull secondObserved"
  check "second retained probe stays independently observable"
    ("ProbeObserved \"second\"" `Text.isInfixOf` output second
      && "second" `Text.isInfixOf` output second)
  void $ turn owner "slowJob <- Cmd.background (Cmd.argv [\"sh\",\"-c\",\"sleep 0.2; printf done\"])\nslowWatcher <- watchSlowCommand me \"owned slow probe\" slowJob 10 64 (\\_ out err -> either (const \"stdout unavailable\") Cmd.pageText out <> either (const \"stderr unavailable\") Cmd.pageText err)"
  void $ turn owner "Cmd.await slowJob"
  watched <- awaitOutput owner
    "slowState <- R.call (slowView (R.client slowWatcher)) ()\ninspectFull slowState"
    (Text.isInfixOf "slowCompletion = Just")
  check "record actor observes the parent's shared job and retains one alert"
    ("slowChecked = True" `Text.isInfixOf` watched
      && "slowAlert = Just" `Text.isInfixOf` watched
      && "slowCompletion = Just" `Text.isInfixOf` watched)
