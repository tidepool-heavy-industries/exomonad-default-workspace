{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | A bounded decision over explicit input. It has no tools or ambient context.
module Examples.ModelCallWorkflow where

import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import qualified Data.Text as Text
import Tidepool.Agent.Contract (ConcurrentEffects, NoTools (..), ContextOrderedEffects, defaultSpec, raiseContextOrdered, specTools)
import qualified Tidepool.Model as Model

noToolsTurn :: Model.Model modelFamily -> Model.ModelTurn modelFamily NoTools effects Text
noToolsTurn model = Model.withLimits (Model.defaultLimits
    { Model.requestLimit = Just 1, Model.toolLimit = Just 0 })
    (Model.textTurn model (defaultSpec { specTools = NoTools })
      "Read only the supplied input. Return one concise conclusion and identify missing evidence. Do not claim to have inspected anything outside this input.")

-- Pass the exact typed selection whose model-call permission was granted.
invokeWithModel :: (Model.KnownFamily modelFamily, Member (Model.ModelCall modelFamily) effects, ConcurrentEffects effects)
  => Model.Model modelFamily -> Text -> Eff effects (Model.ModelResult Text)
invokeWithModel model = Model.invokeModel (noToolsTurn model)

-- Reuse an ordinary base-row program inside a synchronous notebook. Callbacks
-- would still use that base row; this particular turn offers no callbacks.
synchronousWithModel :: (Model.KnownFamily modelFamily, Member (Model.ModelCall modelFamily) effects, ConcurrentEffects effects)
  => Model.Model modelFamily -> Text -> Eff (ContextOrderedEffects effects) (Model.ModelResult Text)
synchronousWithModel model = raiseContextOrdered . invokeWithModel model

-- Keep the original ModelResult bound: this presentation includes the receipt
-- on both success and failure, without converting a typed failure into success.
report :: Model.ModelResult Text
  -> (Either Text Text, Maybe ([(Text, Text)], [Text], [(Text, Int)]))
report result =
  (either (Left . Text.pack . show) Right (Model.modelOutcome result),
    fmap receiptMetadata (Model.modelReceipt result))
  where
    receiptMetadata receipt =
      let usage = Model.invocationUsage receipt
      in ( [("invocation", Model.invocationIdentity receipt),
              ("parent_cell", Model.parentCellIdentity receipt)]
         , Model.requestIdentities receipt
         , [("provider_requests", Model.providerRequests usage),
              ("tool_attempts", Model.toolAttempts usage),
              ("reported_tokens", Model.reportedTokens usage),
              ("requests_without_usage", Model.requestsWithoutUsage usage)] )
