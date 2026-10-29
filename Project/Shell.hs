{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Native bounded command tools, optional after-tool selection, and explicit
-- inspection of frozen retained streams. Automatic selection sees only the
-- bounded display; complete stream reads happen only on explicit inspection.
module Project.Shell
  ( tools,
    afterTool,
    OutputSnapshot,
    outputSnapshot,
    outputSnapshotFor,
    snapshotJob,
    stdoutEndpoint,
    stderrEndpoint,
    SectionId (..),
    OutputSnapshotIssue (..),
    section,
    sectionPage,
    splitSections,
    rawLineThreshold,
  )
where

import Control.Monad.Freer (Eff, Member)
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Project.ContextSelection as Selection
import Tidepool.Agent.Contract (AsServerT, ToolCall(..), ToolResult(..), Annotation(..))
import Tidepool.Aeson.FromJSON (Result(..), fromJSON)
import qualified Tidepool.Command as Cmd
import qualified Tidepool.Command.Tools as Command
import Tidepool.Command.Types (Job (..))
import Tidepool.Effects.Core (Commands, Jev, Reflect)

-- Configuration is ordinary Haskell on purpose: a workspace can tune these
-- values and reload its spec without changing a runtime protocol.
rawLineThreshold, sectionTokens :: Int
-- 'Project.Watchdog.trivialCall' abstains before asking Jev when a
-- finished, non-destructive call's complete frozen payload is no longer than
-- this many lines. Presentation itself no longer gates on line count: output
-- that fits 'max_output_bytes' is shown whole regardless of line count.
rawLineThreshold = 15
sectionTokens = 512

data OutputSnapshot = OutputSnapshot
  { snapshotJob :: Cmd.Job,
    stdoutEndpoint :: Int,
    stderrEndpoint :: Int
  }
  deriving (Eq, Show)

outputSnapshot :: Cmd.Job -> Int -> Int -> OutputSnapshot
outputSnapshot = OutputSnapshot

-- | Recover a frozen snapshot from the command's stable session id.
outputSnapshotFor :: Text -> Int -> Int -> OutputSnapshot
outputSnapshotFor key stdoutEnd stderrEnd = OutputSnapshot (Job key) stdoutEnd stderrEnd

newtype SectionId = SectionId Int
  deriving (Eq, Ord, Show)

data OutputSnapshotIssue
  = SnapshotExpired Cmd.CommandStream Int
  | SnapshotDecodingLoss Cmd.CommandStream Int
  | SnapshotUnavailable Cmd.CommandStream Cmd.CommandError
  | UnknownSection SectionId
  deriving (Eq, Show)

data Section = Section
  { sectionId :: SectionId,
    sectionStream :: Cmd.CommandStream,
    sectionStart :: Int,
    sectionEnd :: Int,
    sectionText :: Text
  }
  deriving (Eq, Show)

tools :: Member Commands effects => Command.ShellTools (AsServerT (Eff effects))
tools = Command.tools

-- | An opt-in focus selects from the already bounded native display. The
-- issued result handle authorizes pruning; frozen stream endpoints remain in
-- the canonical command facts for explicit inspection without replay.
afterTool :: (Member Jev effects, Member Reflect effects)
  => ToolCall -> ToolResult -> Eff effects Annotation
afterTool call result = case (fromJSON (toolResultValue result), fromJSON (toolCallArguments call)) of
  (Success Command.ObservedCommand{}, Success Command.Execute{Command.focus = Just focus}) ->
    Selection.selectRelevantChunks Selection.defaultSelectionConfig focus call result
  _ -> pure NoAnnotation

-- | Deterministic line-aware partitioning. A line longer than one section is
-- split at the scalar boundary; otherwise the last newline within the budget
-- closes the section.
splitSections :: Int -> Text -> [Text]
splitSections tokens = go
  where
    scalars = max 1 (tokens * 4)
    go text
      | T.null text = []
      | T.length text <= scalars = [text]
      | otherwise =
          let prefix = T.take scalars text
              suffix = lastSplit (T.splitOn "\n" prefix)
              lastNewline = scalars - T.length suffix
              cut = if lastNewline == 0 then scalars else lastNewline
              (part, rest) = T.splitAt cut text
           in part : go rest

    lastSplit [] = ""
    lastSplit [part] = part
    lastSplit (_ : parts) = lastSplit parts

loadSections :: Member Commands effects => OutputSnapshot -> Maybe Cmd.CommandOutput -> Eff effects (Either OutputSnapshotIssue [Section])
loadSections frozen initial = do
  out <- readFrozen frozen Cmd.Stdout (stdoutEndpoint frozen) (Cmd.commandStdout <$> initial)
  err <- readFrozen frozen Cmd.Stderr (stderrEndpoint frozen) (Cmd.commandStderr <$> initial)
  pure $ do
    stdoutText <- out
    stderrText <- err
    let pieces = [(Cmd.Stdout, stdoutText), (Cmd.Stderr, stderrText)]
        numbered = assignSections pieces
    pure numbered

assignSections :: [(Cmd.CommandStream, Text)] -> [Section]
assignSections streams = snd (foldl addStream (1, []) streams)
  where
    addStream (nextId, prior) (stream, text) =
      let (_, built) = foldl (addPart stream) (0, []) (splitSections sectionTokens text)
          numbered = zipWith (number stream) [nextId ..] built
       in (nextId + length built, prior <> numbered)
    addPart _ (offset, parts) part =
      let bytes = utf8Bytes part
       in (offset + bytes, parts <> [(offset, offset + bytes, part)])
    number stream ident (start, end, text) = Section (SectionId ident) stream start end text

readFrozen :: Member Commands effects => OutputSnapshot -> Cmd.CommandStream -> Int -> Maybe Cmd.CommandPage -> Eff effects (Either OutputSnapshotIssue Text)
readFrozen frozen stream endpoint initial = case initial of
  Nothing -> collect 0 []
  Just page
    | Cmd.outputLossy page -> pure (Left (SnapshotDecodingLoss stream (Cmd.outputStart page)))
    | Cmd.outputStart page /= 0 || Cmd.outputLostBytes page /= 0 -> pure (Left (SnapshotExpired stream 0))
    | otherwise -> collect (min endpoint (Cmd.outputEnd page)) [Cmd.outputText page]
  where
    collect cursor chunks
      | cursor >= endpoint = pure (Right (T.concat (reverse chunks)))
      | otherwise = do
          let wanted = min 65536 (endpoint - cursor)
          page <- Cmd.tryPage (snapshotJob frozen) stream (Cmd.OutputSlice cursor wanted)
          case page of
            Left issue -> pure (Left (SnapshotUnavailable stream issue))
            Right raw ->
              let details = Cmd.pageDetails raw
               in if Cmd.outputLossy details
                    then pure (Left (SnapshotDecodingLoss stream cursor))
                    else
                      if Cmd.outputStart details /= cursor || Cmd.outputLostBytes details /= 0
                        then pure (Left (SnapshotExpired stream cursor))
                        else
                          if Cmd.outputEnd details <= cursor
                            then pure (Left (SnapshotExpired stream cursor))
                            else collect (min endpoint (Cmd.outputEnd details)) (Cmd.outputText details : chunks)

section :: Member Commands effects => OutputSnapshot -> SectionId -> Eff effects (Either OutputSnapshotIssue Text)
section frozen ident = do
  loaded <- loadSections frozen Nothing
  pure $ do
    sections <- loaded
    maybe (Left (UnknownSection ident)) (Right . sectionText) (findSection ident sections)

sectionPage :: Member Commands effects => OutputSnapshot -> SectionId -> Eff effects (Either OutputSnapshotIssue Cmd.OutputPage)
sectionPage frozen ident = do
  loaded <- loadSections frozen Nothing
  case loaded >>= maybe (Left (UnknownSection ident)) Right . findSection ident of
    Left issue -> pure (Left issue)
    Right found -> do
      page <- Cmd.tryPage (snapshotJob frozen) (sectionStream found) (Cmd.OutputSlice (sectionStart found) (sectionEnd found - sectionStart found))
      pure $ case page of
        Left issue -> Left (SnapshotUnavailable (sectionStream found) issue)
        Right value
          | Cmd.outputLossy (Cmd.pageDetails value) -> Left (SnapshotDecodingLoss (sectionStream found) (sectionStart found))
          | Cmd.outputStart (Cmd.pageDetails value) /= sectionStart found || Cmd.outputLostBytes (Cmd.pageDetails value) /= 0 -> Left (SnapshotExpired (sectionStream found) (sectionStart found))
          | Cmd.outputEnd (Cmd.pageDetails value) /= sectionEnd found -> Left (SnapshotExpired (sectionStream found) (Cmd.outputEnd (Cmd.pageDetails value)))
          | otherwise -> Right value

findSection :: SectionId -> [Section] -> Maybe Section
findSection ident = go
  where
    go [] = Nothing
    go (candidate : rest)
      | sectionId candidate == ident = Just candidate
      | otherwise = go rest

width :: Char -> Int
width character
  | ord character < 0x80 = 1
  | ord character < 0x800 = 2
  | ord character < 0x10000 = 3
  | otherwise = 4

utf8Bytes :: Text -> Int
utf8Bytes = T.foldl' (\n character -> n + width character) 0
