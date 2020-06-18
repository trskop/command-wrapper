let Variables =
      { COMMAND_WRAPPER_LIB : Text
      , COMMAND_WRAPPER_EXEC_LIB : Text
      , COMMAND_WRAPPER_PRELUDE_LIB : Text
      }

let default =
      { COMMAND_WRAPPER_LIB = "${dhallLibDir}/CommandWrapper/package.dhall"
      , COMMAND_WRAPPER_EXEC_LIB = "${dhallLibDir}/Exec/package.dhall"
      , COMMAND_WRAPPER_PRELUDE_LIB = "${dhallLibDir}/Prelude/package.dhall"
      }

let toList
    : Variables → List CommandWrapper.EnvironmentVariable.Type
    =   λ(variables : Variables)
      → Prelude.List.map
          { mapKey : Text, mapValue : Text }
          CommandWrapper.EnvironmentVariable.Type
          (   λ(x : { mapKey : Text, mapValue : Text })
            → { name = x.mapKey, value = x.mapValue }
          )
          (toMap variables)

let consistency0 = assert : { Type = Variables, default }::{=} ≡ default

let consistency1 =
        assert
      :   toList
            { COMMAND_WRAPPER_LIB = "CommandWrapper"
            , COMMAND_WRAPPER_EXEC_LIB = "Exec"
            , COMMAND_WRAPPER_PRELUDE_LIB = "Prelude"
            }
        ≡ [ { name = "COMMAND_WRAPPER_EXEC_LIB", value = "Exec" }
          , { name = "COMMAND_WRAPPER_LIB", value = "CommandWrapper" }
          , { name = "COMMAND_WRAPPER_PRELUDE_LIB", value = "Prelude" }
          ]

in  { Type = Variables, default, toList }
