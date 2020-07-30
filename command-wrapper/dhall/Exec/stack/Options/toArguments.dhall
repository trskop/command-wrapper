-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:b2526c288d5a0419d362c1375b879214396f6d1f6cd88b0416545724fd661b26
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:62f5c48253eed3716701d6112869a8ff41cf60c2e7129024c36b3918d3d33977
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let noArguments = [] : List Text

let toArguments =
      λ(options : Options.Type) →
        optionalOptions
          Text
          (λ(file : Text) → [ "--stack-yaml=${file}" ])
          options.stackYaml

let testEmptyOptions = assert : toArguments Options::{=} ≡ noArguments

in  toArguments : Options.Type → List Text
