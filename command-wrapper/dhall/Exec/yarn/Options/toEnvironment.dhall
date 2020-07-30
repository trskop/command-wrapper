-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:4a4dda0eaf771745c5bfdefb80cdf489c87385626c9501ee19e00485bca4b48f
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:e5843167eab527a6c37c556528ec5ffedb1ba0bf2f0e5f39001b1b1718a3cf1d
          ? ./default.dhall
      }

let Environment =
      { empty =
            ../../../CommandWrapper/Environment/empty sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
          ? ../../../CommandWrapper/Environment/empty
      }

let toEnvironment =
      λ(opts : Options.Type) →
          opts.environment

let test0 = assert : toEnvironment Options::{=} ≡ Environment.empty

in  toEnvironment
