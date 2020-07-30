-- vim: filetype=dhall

let CommonOptions =
        ../../CommonOptions/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ../../CommonOptions/Type.dhall

let ColourOutput =
        ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../../CommandWrapper/ColourOutput/Type

let PsqlOptions =
      { pgpassFile : Optional Text
      , psqlrcFile : Optional Text
      , quiet : Bool
      , verbosity : Optional < default | verbose | terse >
      , colour : Optional ColourOutput
      , showContext : Optional < always | errors | never >
      , editor : Optional Text
      , pager : Optional Text
      }

in  CommonOptions ⩓ PsqlOptions
