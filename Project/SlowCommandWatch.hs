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
  , SlowWatchIssue (..)
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

data SlowWatchIssue
  = NegativeObservationThreshold Int
  | ObservationThresholdTooLong Int
  | InvalidDiagnosticCharacters Int
  deriving (Show, Eq)

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
-- The renderer sees one typed page per stream, including loss and completeness
-- fields; its notice uses `diagnosticCharacters` (at most 8192). Invalid
-- policy is refused before the actor starts.
-- This actor sends one actionable notice if the job is still running
-- after a wait of `thresholdMilliseconds` since this actor attaches
-- (at most 30 seconds), then records any later completion. It does not
-- infer how long the job ran before attachment or start, cancel or retry it.
watchSlowCommand
  :: Member Actor effects
  => AgentRef
  -> Text
  -> Cmd.Job
  -> Int
  -> Int
  -> (Cmd.CommandStatus
      -> Either Cmd.CommandError Cmd.OutputPage
      -> Either Cmd.CommandError Cmd.OutputPage
      -> Text)
  -> Eff effects (Either SlowWatchIssue (ActorHandle SlowCommandWatch))
watchSlowCommand owner context job thresholdMilliseconds diagnosticCharacters render
  | thresholdMilliseconds < 0 = pure (Left (NegativeObservationThreshold thresholdMilliseconds))
  | thresholdMilliseconds > 30000 = pure (Left (ObservationThresholdTooLong thresholdMilliseconds))
  | diagnosticCharacters < 0 || diagnosticCharacters > 8192 =
      pure (Left (InvalidDiagnosticCharacters diagnosticCharacters))
  | otherwise = do
    let specification :: ActorSpec SlowCommandWatch SlowEffects
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
                    (Cmd.Observation thresholdMilliseconds 0) job
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
                          let diagnostic = Text.take diagnosticCharacters $ render latest
                                stdoutPage stderrPage
                          beforeNotice <- Cmd.quiet $ Cmd.observe (Cmd.Observation 0 0) job
                          case beforeNotice of
                            Cmd.CommandFinished result ->
                              R.modify' (\state -> state { slowCompletion = Just result })
                            _ -> do
                              receipt <- sendMessage owner $ Text.unlines
                                [ "Slow command: " <> context
                                , "Status after " <> Text.pack (show thresholdMilliseconds)
                                    <> " ms: " <> Text.pack (show beforeNotice)
                                , diagnostic
                                ]
                              R.modify' (\state -> state
                                { slowAlert = Just (SlowAlert beforeNotice diagnostic receipt) })
          , slowCompleted = R.on (Cmd.completion job) $ \result ->
              R.modify' (\state -> state { slowCompletion = Just result })
          }
    actor <- R.start specification
    R.send (slowCheck (R.client actor)) ()
    pure (Right actor)
