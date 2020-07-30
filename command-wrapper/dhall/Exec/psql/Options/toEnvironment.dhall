-- vim: filetype=dhall
-- 
-- Notes:
-- 
-- * Be avare that environment variable `PG_COLOR` is supported by `psql` since
--   PostgreSQL version 12. Earlier versions will ignore it.
--
-- * Some environment variables like `PSQL_PAGER` are supported only by newer
--   versions as well. Older versions, in case of mentioned variable, still
--   support `PAGER`. For that reason we export both variants.

let Options =
      { Type =
            ./Type.dhall sha256:61553461550695b5b8c52861fff9ceecf0c70ebd703cfa2f7465d97aca6d3a66
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:e105968e9739efb82a2a48c7bf6e15949e1d0b511dca30e2e2deb5a16e76a1fb
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

let ColourOutput =
      { Type =
            ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
          ? ../../../CommandWrapper/ColourOutput/Type
      , toText =
            ../../../CommandWrapper/ColourOutput/toText sha256:51d22acdc9b32f757e9170ae7c2cf244ce0488aa839dada9ee38bb3f1ee4d7bf
          ? ../../../CommandWrapper/ColourOutput/toText
      }

let toEnvironment =
      λ(options : Options.Type) →
          CommonOptions.toEnvironment options.{ workingDirectory, environment }
        # merge
            { None = Environment.empty
            , Some = λ(value : Text) → [ { name = "PGPASSFILE", value } ]
            }
            options.pgpassFile
        # merge
            { None = Environment.empty
            , Some = λ(value : Text) → [ { name = "PSQLRC", value } ]
            }
            options.psqlrcFile
        # merge
            { None = Environment.empty
            , Some =
                λ(colourOutput : ColourOutput.Type) →
                  [ { name = "PG_COLOR"
                    , value = ColourOutput.toText colourOutput
                    }
                  ]
            }
            options.colour
        # merge
            { None = Environment.empty
            , Some =
                λ(value : Text) →
                  [ { name = "PSQL_EDITOR", value }
                  , { name = "EDITOR", value }
                  , { name = "VISUAL", value }
                  ]
            }
            options.editor
        # merge
            { None = Environment.empty
            , Some =
                λ(value : Text) →
                  [ { name = "PSQL_PAGER", value }, { name = "PAGER", value } ]
            }
            options.pager

let emptyEnvironmnetByDefault =
      assert : toEnvironment Options.default ≡ Environment.empty

in  toEnvironment : Options.Type → Environment.Type
