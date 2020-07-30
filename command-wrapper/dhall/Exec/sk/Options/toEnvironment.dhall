-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:9c93aeda7d25765472107935265ba78477d16d1787b33f0d6db56d27174bb532
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:49a9f1ae25424574c1694747822da1b8627010c47f18d495e197a9ca36c317b7
          ? ./default.dhall
      }

let Environment =
      { empty =
            ../../../CommandWrapper/Environment/empty sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
          ? ../../../CommandWrapper/Environment/empty
      }

let toEnvironment = λ(opts : Options.Type) → opts.environment

let test0 = assert : toEnvironment Options::{=} ≡ Environment.empty

in  toEnvironment
