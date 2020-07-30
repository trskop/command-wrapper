-- vim: filetype=dhall

let Options =
        ./Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ./Type.dhall

let Environment =
      { empty =
            ../../CommandWrapper/Environment/empty sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
          ? ../../CommandWrapper/Environment/empty
      }

let default = { workingDirectory = None Text, environment = Environment.empty }

let consistency = assert : { Type = Options, default }::{=} ≡ default

in  default
