-- vim: filetype=dhall

let CommonOptions =
        ../../CommonOptions/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ../../CommonOptions/Type.dhall

in    CommonOptions
    ⩓ { noPager : Bool
      , noRun : Bool
      }
