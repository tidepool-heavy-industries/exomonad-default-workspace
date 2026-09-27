{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.MergeChecks (redPreserved) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import qualified Data.Text as Text
import Tidepool.Check

-- A failed integration check leaves its staged and unstaged diagnostics in
-- the managed checkout while the named publication branch stays at the
-- previous green head. Its next publish is refused by branch drift.
redPreserved :: Member RecipeCheck effects => Eff effects ()
redPreserved = do
  owner <- root
  before <- git owner ["rev-parse", "HEAD"]
  let branch = "recipe/red-preserved"
  void $ git owner ["branch", branch, before]
  created <- turn owner $ Text.unlines
    [ "import qualified Project.Merge as M"
    , "Right integration <- createWorktree (fromRef \"recipe/red-preserved\" \"red-preserved\")"
    , "merger <- R.start (M.mergeInto (worktreeId integration) (Just \"recipe/red-preserved\") [\"sh\", \"-c\", \"printf 'staged-check\\n' > red-preserved.txt; git add -- red-preserved.txt; printf 'working-check\\n' > red-preserved.txt; printf intentional-red >&2; exit 7\"])"
    , "worktreeId integration"
    ]
  check "the integration actor owns a managed checkout"
    ("WorktreeId" `Text.isInfixOf` lastOutput created)
  pathResult <- turn owner "cwd (handleReceipt integration)"
  let integrationPath = Text.dropAround (== '"') (Text.strip (lastOutput pathResult))
  candidate <- checkpoint owner "red-preserved.txt" "candidate\n" "red preservation candidate"
  result <- turn owner $ Text.unlines
    [ "let request = M.PublishRequest \"red preservation\" " <> gitOidLiteral candidate <> " \"merge red preservation candidate\""
    , "first <- R.call (M.publish (R.client merger)) request"
    , "headAfter <- worktreeHead integration"
    , "second <- R.call (M.publish (R.client merger)) request"
    , "(first, headAfter == " <> gitOidLiteral candidate <> ", second)"
    ]
  let observed = lastOutput result
  check "the red result retains the previous and checked heads plus failed check evidence"
    (all (`Text.isInfixOf` observed)
      ["RedPreserved", before, candidate, "intentional-red"])
  check "the integration checkout stays on the red head and the next publish is refused"
    ("True,MergeBlocked" `Text.isInfixOf` Text.filter (/= ' ') observed
      && "publication branch" `Text.isInfixOf` observed)
  published <- git owner ["rev-parse", "refs/heads/" <> branch]
  check "the named publication branch remains at the previous green head"
    (published == before)
  status <- git owner ["-C", integrationPath, "status", "--short", "--", "red-preserved.txt"]
  staged <- git owner ["-C", integrationPath, "diff", "--cached", "--", "red-preserved.txt"]
  working <- git owner ["-C", integrationPath, "diff", "--", "red-preserved.txt"]
  check "the failed check's staged and working edits survive in the integration checkout"
    (status == "MM red-preserved.txt"
      && "staged-check" `Text.isInfixOf` staged
      && "working-check" `Text.isInfixOf` working)
  reflog <- git owner ["-C", integrationPath, "reflog", "--format=%gs", "-3"]
  check "the red path never records a destructive reset"
    (not ("reset:" `Text.isInfixOf` reflog))
