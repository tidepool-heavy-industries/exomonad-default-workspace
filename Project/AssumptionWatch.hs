{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Watch an existing source for a change to one caller-selected assumption.
-- The source owner retains its ordering and lifetime; this actor remembers the
-- latest observation and the last actionable change, without polling.
module Project.AssumptionWatch
  ( AssumptionWatch (assumptionView)
  , AssumptionState (..)
  , AssumptionChange (..)
  , BaselineStatus (..)
  , assumptionDefinition
  , watchAssumption
  , watchIncorporatedBaseline
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Tidepool.Actor.Record as R
import Tidepool.Actors.Exomonad
import Tidepool.Effects.Core (Actor)
import Tidepool.Worktree (renderGitOid)
import Project.Actors (CoordinationEffects, coordinationActor)
import Project.Types (Incorporation (..))

data AssumptionChange fingerprint = AssumptionChange
  { beforeFingerprint :: fingerprint
  , afterFingerprint :: fingerprint
  , beforeContext :: Text
  , afterContext :: Text
  , changeReason :: Text
  , changeNotice :: Either NotificationError NotificationReceipt
  }

data AssumptionState fingerprint = AssumptionState
  { assumptionCurrent :: (fingerprint, Text)
  , assumptionLastChange :: Maybe (AssumptionChange fingerprint)
  , assumptionChangeCount :: Int
  }

data AssumptionWatch fingerprint observation mode = AssumptionWatch
  { assumptionState :: mode :- State (AssumptionState fingerprint)
  , assumptionView :: mode :- Call () (R.Reply (AssumptionState fingerprint))
  , assumptionEvents :: mode :- Event observation
  } deriving Generic

type AssumptionEffects fingerprint observation =
  CoordinationEffects (AssumptionWatch fingerprint observation)

data BaselineStatus
  = BaselineAt GitOid
  | BaselineUnavailable Text
  deriving (Show, Eq)

-- | The projection selects the relevant observation and its fingerprint.
-- Equal fingerprints suppress notices. A changed fingerprint updates the
-- remembered context; the callback sees typed before/after observations to
-- decide whether that change needs action.
assumptionDefinition
  :: Eq fingerprint
  => AgentRef
  -> (fingerprint, Text)
  -> R.EventSource observation
  -> (observation -> Maybe (fingerprint, Text))
  -> ((fingerprint, Text) -> (fingerprint, Text) -> Maybe Text)
  -> ActorSpec (AssumptionWatch fingerprint observation) (AssumptionEffects fingerprint observation)
assumptionDefinition owner initial source project relevant =
  coordinationActor "assumption-watch" AssumptionWatch
    { assumptionState = AssumptionState initial Nothing 0
    , assumptionView = \() -> R.get
    , assumptionEvents = R.on source $ \observation -> case project observation of
        Nothing -> pure ()
        Just current -> do
          prior <- R.gets assumptionCurrent
          if fst prior == fst current
            then R.modify' (\state -> state { assumptionCurrent = current })
            else case relevant prior current of
              Nothing -> R.modify' (\state -> state { assumptionCurrent = current })
              Just reason -> do
                receipt <- sendMessage owner (Text.unlines
                  [ "Assumption changed: " <> reason
                  , "Before: " <> snd prior
                  , "After: " <> snd current
                  ])
                R.modify' $ \state -> state
                  { assumptionCurrent = current
                  , assumptionLastChange = Just (AssumptionChange
                      (fst prior) (fst current) (snd prior) (snd current) reason receipt)
                  , assumptionChangeCount = assumptionChangeCount state + 1
                  }
    }

watchAssumption
  :: (Eq fingerprint, Member Actor effects)
  => AgentRef
  -> (fingerprint, Text)
  -> R.EventSource observation
  -> (observation -> Maybe (fingerprint, Text))
  -> ((fingerprint, Text) -> (fingerprint, Text) -> Maybe Text)
  -> Eff effects (ActorHandle (AssumptionWatch fingerprint observation))
watchAssumption owner initial source project relevant =
  R.start (assumptionDefinition owner initial source project relevant)

-- | A pending child's submitted baseline is a concrete first consumer. The
-- caller supplies the exact incorporation response and the child context;
-- a successful source advance or unavailable incorporation asks its owner to
-- revisit that child. Unavailable and unchanged remain distinct states.
watchIncorporatedBaseline
  :: Member Actor effects
  => AgentRef -> GitOid -> Text -> Response Incorporation
  -> Eff effects (ActorHandle (AssumptionWatch BaselineStatus (Either ResponseFailure (ResponseResult Incorporation))))
watchIncorporatedBaseline owner baseline pendingChild incorporation =
  watchAssumption owner (BaselineAt baseline, pendingChild <> " at " <> renderGitOid baseline)
    (R.settlement incorporation) project relevant
  where
    project (Right result) = case responseValue result of
      Incorporated _ headOid _ ->
        Just (BaselineAt headOid, pendingChild <> " at " <> renderGitOid headOid)
      IncorporationBlocked _ reason evidence ->
        Just (BaselineUnavailable reason, pendingChild <> " blocked: "
          <> reason <> "; evidence: " <> Text.intercalate ", " evidence)
    project (Left failure) =
      Just (BaselineUnavailable (Text.pack (show failure)), pendingChild
        <> " incorporation failed: " <> Text.pack (show failure))
    relevant _ current = Just $ case fst current of
      BaselineAt _ -> "incorporated source changed while a child is pending"
      BaselineUnavailable _ -> "pending child incorporation became unavailable"
