-- vim: filetype=dhall

let LogLevel =
        ../LogLevel/Type.dhall sha256:f63d8d8d8c27e7edd9b623e5abea25b28f94293d2461347c718782ae09dfc41b
      ? ../LogLevel/Type.dhall

let CommonOptions =
        ../../CommonOptions/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ../../CommonOptions/Type.dhall

let DockerGlobalOptions =
      { host : List Text
      , logLevel : Optional LogLevel
      , config : Optional Text
      , context : Optional Text
      , debug : Bool
      }

in  CommonOptions ⩓ DockerGlobalOptions : Type
