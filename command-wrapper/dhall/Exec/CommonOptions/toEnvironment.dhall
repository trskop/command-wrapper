-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ./default.dhall
      }

let Environment =
      { Type =
            ../../CommandWrapper/Environment/Type sha256:bfc2cb080bb0cac5a42a81beb707437389fa9c6f8e54ae8dd7ce09c6566140ee
          ? ../../CommandWrapper/Environment/Type
      , empty =
            ../../CommandWrapper/Environment/empty sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
          ? ../../CommandWrapper/Environment/empty
      }

let toEnvironment = λ(_ : Options.Type) → _.environment

let test0 = assert : toEnvironment Options::{=} ≡ Environment.empty

in  toEnvironment : Options.Type → Environment.Type
