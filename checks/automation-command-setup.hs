import qualified Tidepool.Command as Cmd
import qualified Tidepool.Actor.Record as R
import Project.ParallelInvestigate
import Project.SlowCommandWatch
let commandProbes =
      [ CommandProbe "first" "first read" "/tmp" (Cmd.MiB 64)
          (Cmd.argv ["sh", "-c", "printf first"])
      , CommandProbe "second" "second read" "/tmp" (Cmd.MiB 64)
          (Cmd.argv ["sh", "-c", "printf second"])
      ]
launched <- startProbeBatch (ProbeLimits 2 2) commandProbes ["first", "second"]
