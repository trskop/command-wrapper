-- vim: filetype=dhall

λ ( libraries
  : { prelude : Optional Text
    , commandWrapper : Optional Text
    , exec : Optional Text
    }
  ) →
  ''
  -- vim: filetype=dhall
  --
  -- TODO: Description

  ${merge
      { None = ""
      , Some =
          λ(_ : Text) →
            ''
            let Prelude = ${_}

            ''
      }
      libraries.prelude}
  ${merge
      { None = ""
      , Some =
          λ(_ : Text) →
            ''
            let CommandWrapper = ${_}

            ''
      }
      libraries.commandWrapper}
  ${merge
      { None = ""
      , Some =
          λ(_ : Text) →
            ''
            let Exec = ${_}

            ''
      }
      libraries.exec}
  let -- Smart constructor for configuration that allows us to use defaults.
      Config =
        { Type = {}
        , default = {=}
        }

  in  Config::{=}
  ''
