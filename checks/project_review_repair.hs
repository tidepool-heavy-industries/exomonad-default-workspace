next <- repair "repair-candidate" sessionInput (reviewInput sessionInput) ["preserve the product gate"]
let Right (Right revision) = next
let repairedLabel = "repaired" :: WatchLabel
revisionRetention <- detachRequest revision
repaired <- case revisionRetention of
  Right () -> watch repairedLabel (awaitSettled revision)
  Left issue -> error (T.pack (show issue))
