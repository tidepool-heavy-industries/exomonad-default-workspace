{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Explicit child delegation used by the recursive request examples.
module Project.RecursiveSpec (RecursiveEffects, recursiveSpec, captureContext) where

import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import Tidepool.Agent.Contract (AgentSpec)
import Tidepool.Actors.Exomonad (ContextCheckpoint, CheckpointRefusal, checkpoint)
import Tidepool.Model (Model, KnownFamily, ForkAgent, SpawnAgent)
import qualified AgentSpec as Installed
import qualified Project.Tools as Tools
import Project.Work (WorkspaceEffects)

type RecursiveEffects family = ForkAgent family ': SpawnAgent family ': WorkspaceEffects

recursiveSpec :: KnownFamily family => Model family
  -> AgentSpec (Tools.WorkspaceTools (RecursiveEffects family)) (RecursiveEffects family)
recursiveSpec _ = Installed.agentSpec

-- Keep the checkpoint family tied to the ordinary actor-local selection value.
captureContext :: forall family effects.
  (KnownFamily family, Member (ForkAgent family) effects)
  => Model family -> Text -> Eff effects (Either CheckpointRefusal (ContextCheckpoint family))
captureContext _ = checkpoint @family
