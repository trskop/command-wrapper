-- vim: filetype=dhall
--
-- See `ssh(1)` documentation, especially:
--
-- *   Description of `-N` option to understand 'doNotExecuteRemoteCommand`
--     field.
-- *   Description of `-n` option to understand `preventReadingOfStdin`, which
--     redirects stdin from `/dev/null`.

let CommonOptions =
        ../../CommonOptions/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ../../CommonOptions/Type.dhall

let Forwarding =
        ../Forwarding/Type.dhall sha256:434b5e6be35d6dd97cd29665c8c8855be653a25396174de07ec3c158dd8e4cdb
      ? ../Forwarding/Type.dhall

let SshOptions =
      { forwardings : List Forwarding
      , identityFile : Optional Text
      , configFile : Optional Text
      , allocatePseudoTerminal : Optional Bool
      , doNotExecuteRemoteCommand : Bool
      , preventReadingOfStdin : Bool
      }

in  CommonOptions ⩓ SshOptions
