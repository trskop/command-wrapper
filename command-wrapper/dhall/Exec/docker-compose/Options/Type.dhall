-- vim: filetype=dhall
--
-- Be aware that `workingDirectory` and `environment` are here for convenience
-- when building `ExecCommand` and are not made into command line options when
-- `toArguments` is called.

let LogLevel =
        ../LogLevel/Type.dhall sha256:653f22f10a9c744a6b718ddd98eb1c166d430edf82d5e201b971e798c52b5210
      ? ../LogLevel/Type.dhall

let CommonOptions =
        ../../CommonOptions/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ../../CommonOptions/Type.dhall

let DockerComposeGlobalOptions =
      { files : List Text
      , projectName : Optional Text
      , projectDirectory : Optional Text
      , contextName : Optional Text
      , environmentFile : Optional Text
      , logLevel : Optional LogLevel
      , noAnsi : Bool
      }

in  CommonOptions ⩓ DockerComposeGlobalOptions : Type
