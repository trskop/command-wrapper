-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:7cbb3d15d2d57539fe0274c8c1223148bb7b34d0310e0c58d4ab7f49e7e0ca5f
          ? ./Type.dhall
      }

let Verbosity =
        ../../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../../CommandWrapper/Verbosity/Type

let ColourOutput =
        ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../../CommandWrapper/ColourOutput/Type

let default =
      { verbosity = None Verbosity
      , colour = None ColourOutput
      , allowAliases = None Bool
      , changeDirectory = None Text
      , invokeAs = None Text
      , path = None Text
      , manPath = None Text
      , configHome = None Text
      }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
