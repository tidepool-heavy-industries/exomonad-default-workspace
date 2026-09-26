# Focused test helper seed

The root actor starts with these files in its `.exomonad/helpers/` draft. Edit
them there and publish with `reload_helpers`; forked actors inherit the draft
as it stands when they fork. Customize the
package, target, filter, expected count and diagnostic alternatives for the
component under test. The helper starts `scripts/cargo-focused-test` as a
background command; its retained JSON contains the selected artifact, source,
matched tests, execution summaries and output log path.

Call `startFocused` with a `FocusedSpec`, wait for the returned job with
`Cmd.awaitFinished`, then call `finishFocused` on the same `FocusedRun`.
`focusedPassed` checks exit, clean source identity, selected count and executed
count in code. On an unexpected command failure, Jev classifies the retained
diagnostic; an unavailable or doubtful judgment stays in `focusedFailure` and
does not erase the command or JSON evidence. Missing JSON is reported as
missing evidence. The caller should read the completion report's source and
cleanup before relying on the result.
