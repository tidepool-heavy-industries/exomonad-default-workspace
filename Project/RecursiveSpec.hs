{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Explicit child delegation used by the recursive request examples.
module Project.RecursiveSpec (RecursiveEffects, recursiveSpec) where

import Tidepool.Agent.Contract (AgentSpec)
import Tidepool.Model (Model, KnownFamily, ForkAgent, SpawnAgent)
import qualified AgentSpec as Installed
import qualified Project.Tools as Tools
import Project.Work (WorkspaceEffects)

type RecursiveEffects modelFamily = ForkAgent modelFamily ': SpawnAgent modelFamily ': WorkspaceEffects

recursiveSpec :: KnownFamily modelFamily => Model modelFamily
  -> AgentSpec (Tools.WorkspaceTools (RecursiveEffects modelFamily)) (RecursiveEffects modelFamily)
recursiveSpec _ = Installed.agentSpec
