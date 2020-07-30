-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:d10faab8c8388e68d0e931248dee4741cbcef8b593095d8dd8a42a54c2272ab5
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:6bcdf525021173128fe71180d9f1a2e99cf514d0ab07bf391693d107e846125f
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
