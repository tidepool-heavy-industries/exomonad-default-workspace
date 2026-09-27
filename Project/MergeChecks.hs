{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.MergeChecks (redPreserved) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import qualified Data.Text as Text
import Tidepool.Check

-- A failed integration check leaves the exact checked commit in the managed
-- checkout while the named publication branch stays at the previous green
-- head. Its next publish is refused by the existing branch-drift check.
redPreserved :: Member RecipeCheck effects => Eff effects ()
redPreserved = do
  owner <- root
  before <- git owner ["rev-parse", "HEAD"]
  let branch = "recipe/red-preserved"
  void $ git owner ["branch", branch, before]
  created <- turn owner $ Text.unlines
    [ "import qualified Project.Merge as M"
    , "Right integration <- createWorktree (fromRef \"recipe/red-preserved\" \"red-preserved")"
    , "merger <- R.start (M.mergeInto (worktreeId integration) (Just \"recipe/red-preserved\") [\"sh\", \"-c\", \"printf intentional-red >&2; exit 7\"])"
    , "worktreeId integration"
    ]
  check "the integration actor owns a managed checkout"
    ("WorktreeId" `Text.isInfixOf` lastOutput created)
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
