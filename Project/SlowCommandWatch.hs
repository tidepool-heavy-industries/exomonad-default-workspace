{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | One bounded, independent observation episode for an already-started job.
module Project.SlowCommandWatch
  ( SlowCommandWatch (slowView)
  , SlowState (..)
  , SlowAlert (..)
  , watchSlowCommand
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Tidepool.Actor as Actor
import qualified Tidepool.Actor.Record as R
import Tidepool.Actors.Exomonad
import qualified Tidepool.Command as Cmd
import Tidepool.Effects.Core (Actor, Commands, Notifications)
import Tidepool.Effects.Row (knownEffects)

data SlowAlert = SlowAlert
  { alertStatus :: Cmd.CommandStatus
  , alertDiagnostic :: Text
  , alertReceipt :: Either NotificationError NotificationReceipt
  } deriving (Show)

data SlowState = SlowState
  { slowJob :: Cmd.Job
  , slowChecked :: Bool
  , slowCompletion :: Maybe Cmd.CommandResult
  , slowAlert :: Maybe SlowAlert
  } deriving (Show)

data SlowCommandWatch mode = SlowCommandWatch
  { slowState :: mode :- State SlowState
  , slowView :: mode :- Call () (R.Reply SlowState)
  , slowCheck :: mode :- Call () NoReply
  , slowCompleted :: mode :- Event Cmd.CommandResult
  } deriving Generic

type SlowEffects = R.LocalEffects SlowCommandWatch '[Commands, Notifications, Actor]

-- | The caller owns the supplied Job and supplies a pure diagnostic renderer.
-- The renderer sees at most `diagnosticCharacters` from each retained first
-- page. This actor sends one actionable notice if the job is still running
-- after a wait of up to `thresholdMilliseconds` since this actor attaches
-- (capped at 30 seconds), then records any later completion. It does not
-- infer how long the job ran before attachment or start, cancel or retry it.
watchSlowCommand
  :: Member Actor effects
  => AgentRef
  -> Text
  -> Cmd.Job
  -> Int
  -> Int
  -> (Cmd.CommandStatus -> Text -> Text -> Text)
  -> Eff effects (ActorHandle SlowCommandWatch)
watchSlowCommand owner context job thresholdMilliseconds diagnosticCharacters render = do
  let threshold = max 0 (min 30000 thresholdMilliseconds)
      specification :: ActorSpec SlowCommandWatch SlowEffects
      specification = R.definition "slow-command-watch" (Actor.Selected knownEffects)
        SlowCommandWatch
        { slowState = SlowState job False Nothing Nothing
      , slowView = \() -> R.get
      , slowCheck = \() -> do
          current <- R.get
          if slowChecked current
            then pure ()
            else do
              R.modify' (\state -> state { slowChecked = True })
              observed <- Cmd.quiet $ Cmd.observe
                (Cmd.Observation threshold 0) job
              case observed of
                Cmd.CommandFinished result ->
                  R.modify' (\state -> state { slowCompletion = Just result })
                _ -> do
                  latest <- Cmd.quiet $ Cmd.observe (Cmd.Observation 0 0) job
                  case latest of
                    Cmd.CommandFinished result ->
                      R.modify' (\state -> state { slowCompletion = Just result })
                    _ -> do
                      stdoutPage <- Cmd.tryPage job Cmd.Stdout Cmd.OutputBeginning
                      stderrPage <- Cmd.tryPage job Cmd.Stderr Cmd.OutputBeginning
                      let chars = max 0 (min 4096 diagnosticCharacters)
                          excerpt = Text.take chars . either (Text.pack . show) Cmd.pageText
                          diagnostic = Text.take 8192 $ render latest
                            (excerpt stdoutPage) (excerpt stderrPage)
                      receipt <- sendMessage owner $ Text.unlines
                        [ "Slow command: " <> context
                        , "Status after " <> Text.pack (show threshold)
                            <> " ms: " <> Text.pack (show latest)
                        , diagnostic
                        ]
                      R.modify' (\state -> state
                        { slowAlert = Just (SlowAlert latest diagnostic receipt) })
      , slowCompleted = R.on (Cmd.completion job) $ \result ->
          R.modify' (\state -> state { slowCompletion = Just result })
        }
  actor <- R.start specification
  R.send (slowCheck (R.client actor)) ()
  pure actor
