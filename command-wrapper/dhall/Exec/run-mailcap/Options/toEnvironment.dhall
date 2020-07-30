-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:674f9d48d01ccb653cd071dd30af0766337d30d0c8fa11498ab3a6e91219479a
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:6c7eb164746af8d0c1106285fdb390265b46ae49928384734675638088522e32
          ? ./default.dhall
      }

let CommonOptions =
      { toEnvironment =
            ../../CommonOptions/toEnvironment.dhall sha256:6fa0057412ff539cd27b042cfd1a59fcee06eb64fbff36ca904f0a3436619d35
          ? ../../CommonOptions/toEnvironment.dhall
      }

let Environment =
      { Type =
            ../../../CommandWrapper/Environment/Type sha256:bfc2cb080bb0cac5a42a81beb707437389fa9c6f8e54ae8dd7ce09c6566140ee
          ? ../../../CommandWrapper/Environment/Type
      , empty =
            ../../../CommandWrapper/Environment/empty sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
          ? ../../../CommandWrapper/Environment/empty
      }

let toEnvironment =
      λ(_ : Options.Type) →
        CommonOptions.toEnvironment _.{ workingDirectory, environment }

let test0 = assert : toEnvironment Options::{=} ≡ Environment.empty

in  toEnvironment : Options.Type → Environment.Type
