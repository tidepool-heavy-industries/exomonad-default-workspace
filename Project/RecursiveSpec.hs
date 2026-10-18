{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Explicit child delegation used by the recursive request examples.
module Project.RecursiveSpec (RecursiveEffects, recursiveSpec) where

import Tidepool.Agent.Contract (AgentSpec)
import Tidepool.Model (Model, KnownFamily, ForkAgent, SpawnAgent)
import qualified AgentSpec as Installed
import qualified Project.Tools as Tools
import Project.Work (WorkspaceEffects)

type RecursiveEffects family = ForkAgent family ': SpawnAgent family ': WorkspaceEffects

recursiveSpec :: KnownFamily family => Model family
  -> AgentSpec (Tools.WorkspaceTools (RecursiveEffects family)) (RecursiveEffects family)
recursiveSpec _ = Installed.agentSpec
