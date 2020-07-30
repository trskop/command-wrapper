-- vim: filetype=dhall
--
-- See `command-wrapper(1)` for more information.

let Verbosity =
        ../../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../../CommandWrapper/Verbosity/Type

let ColourOutput =
        ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../../CommandWrapper/ColourOutput/Type

in  -- `--verbosity=VERBOSITY`
    { verbosity : Optional Verbosity

    -- `--colour=COLOUR`
    , colour : Optional ColourOutput

    -- `--[no-]aliases]`
    , allowAliases : Optional Bool

    -- `--change-directory=DIRECTORY`
    , changeDirectory : Optional Text

    -- Value of `COMMAND_WRAPPER_INVOKE_AS` environment variable.
    , invokeAs : Optional Text

    -- Value of `COMMAND_WRAPPER_PATH` environment variable.
    , path : Optional Text

    -- Value of `COMMAND_WRAPPER_MANPATH` environment variable.
    , manPath : Optional Text

    -- Value of `XDG_CONFIG_HOME` environment variable.
    , configHome : Optional Text
    }
