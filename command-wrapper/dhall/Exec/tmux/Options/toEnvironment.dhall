-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:b583193040d153654a70a31291aa959845a9d3246c367e65567b3d96dd3c6730
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:c194361ef28684b8a55c18d6e9176f2b7c77878cb9f52a94cc568b70f9695f55
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
        # merge
            { None = Environment.empty
            , Some = λ(value : Text) → [ { name = "TERM", value } ]
            }
            opts.term

let test0 = assert : toEnvironment Options::{=} ≡ Environment.empty

let test1 =
        assert
      :   toEnvironment Options::{ term = Some "xterm-256color" }
        ≡ [ { name = "TERM", value = "xterm-256color" } ]

in  toEnvironment
