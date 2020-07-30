-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:5c7701e806aeb7526088ef25f1233f2237342a4b1fd617a028f1258983d5202c
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:607cce48bb9e32e0d36aca288ca9b7aef0377d7c64f09f22e5445fd846782320
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
