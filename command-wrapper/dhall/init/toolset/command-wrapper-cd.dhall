-- vim: filetype=dhall

λ(toolsetName : Text) →
λ(library : { prelude : Text, commandWrapper : Text, exec : Text }) →
λ(runtimeDirectory : { libDir : Text, manDir : Text }) →
  ''
  let CommandWrapper =
        ${library.commandWrapper}

  --let Exec =
  --      ${library.exec}

  --let Prelude =
  --      ${library.prelude}

  let empty = [] : List Text

  let directories =
          ./cd/directories-common.dhall
        # (./cd/directories.dhall ? empty)
        # (./cd/directories-local.dhall ? empty)

  let global =
          ../command-wrapper/command-wrapper-cd.dhall
        ? CommandWrapper.CdConfig::{=}

  in  global ⫽ { directories = global.directories # directories }
  ''
