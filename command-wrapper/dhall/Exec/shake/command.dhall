-- vim: filetype=dhall
--
-- ```
-- SHAKE_EXECUTABLE [OPTIONS] [ARGUMENTS]
-- ```

let Options =
        ./Options/package.dhall sha256:624ef494d005346cadebdf9c5c6600e2a35e5f1d4c51123437d938cfc9359c49
      ? ./Options/package.dhall

let ShakeVerbosity =
        ./ShakeVerbosity/package.dhall sha256:3d0bf0eede2a1cc535bb505f3980db0a6757565bf82891c161ce60cff11d4363
      ? ./ShakeVerbosity/package.dhall

let ColourOutput =
        ../../CommandWrapper/ColourOutput/package.dhall sha256:8fd5555f273cd4424f8fe2003154c4e36ea657bb5a2642d3677c4756b14c32e8
      ? ../../CommandWrapper/ColourOutput/package.dhall

let Verbosity =
        ../../CommandWrapper/Verbosity/package.dhall sha256:488f95a5a27b82653c5a759b592b08e16940d1698dcf956fcbd9c153cb2547f2
      ? ../../CommandWrapper/Verbosity/package.dhall

let ExecCommand =
        ../../CommandWrapper/ExecCommand/package.dhall sha256:3d1f1db23e794116bfc80efad94a291087792b0a96d8f5be5b1c9276657d0663
      ? ../../CommandWrapper/ExecCommand/package.dhall

let fromOptional =
        ../../CommandWrapper/Optional/from sha256:5bd665b0d6605c374b3c4a7e2e2bd3b9c1e39323d41441149ed5e30d86e889ad
      ? ../../CommandWrapper/Optional/from

let updateOptions =
      λ(options : Options.Type) →
      λ(colourOutput : ColourOutput.Type) →
          options
        ⫽ { colour =
              ColourOutput.fold
                Bool
                { Always = True, Auto = True, Never = False }
                colourOutput
          }

let -- Since Shake is not one specific executable we need to pass the specific
    -- command as an argument.
    --
    -- Be aware that:
    --
    -- * The value of `colour` `options : Options.Type` is overriden by Command
    --   Wrapper's `colourOutput : ColourOutput.Type` value. If you don't want
    --   that then -- construct `ExecCommand.Type` yourself and reuse
    --   `Options.toArguments`.
    --
    -- * The value of `verbosity` in `options : Options.Type` is used as a
    --   default `ShakeVerbosity.Type` value when evaluating Command Wrapper's
    --   `verbosity : Verbosity.Type`.  What it means is that we treat it as
    --   the default value set in the Shake binary and then modify it based on
    --   Command Wrapper's `verbosity : Verbosity.Type` value. If you don't
    --   want this behaviour construct `ExecCommand.Type` yourself and reuse
    --   `Options.toArguments`.
    command =
      λ(command : Text) →
      λ(options : Options.Type) →
      λ(arguments : List Text) →
      λ(verbosity : Verbosity.Type) →
      λ(colourOutput : ColourOutput.Type) →
      λ(userArguments : List Text) →
        ExecCommand::{
        , command
        , arguments =
              Options.toArguments
                ( fromOptional
                    ShakeVerbosity.Type
                    ShakeVerbosity.default
                    options.verbosity
                )
                (updateOptions options colourOutput)
            # arguments
            # userArguments
        , environment = options.environment
        }

in    command
    : ∀(command : Text) →
      ∀(options : Options.Type) →
      ∀(arguments : List Text) →
      ∀(verbosity : Verbosity.Type) →
      ∀(colourOutput : ColourOutput.Type) →
      ∀(userArguments : List Text) →
        ExecCommand.Type
