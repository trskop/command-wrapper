-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:82c559b2efa14b05efa1644a6ac53852c45823fd1395f1aa223fd8f9aa549b5c
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:9045715df45ae4d2438d75ea7c934cb3e113716be3e88c52b34a18dfadb8a1a2
          ? ./default.dhall
      }

let LogLevel =
      { Type =
            ../LogLevel/Type.dhall sha256:653f22f10a9c744a6b718ddd98eb1c166d430edf82d5e201b971e798c52b5210
          ? ../LogLevel/Type.dhall
      , toText =
            ../LogLevel/toText.dhall sha256:c10bd400246ab8b8819a817759416dfd26f53068a507bc8f3b11b6916a553803
          ? ../LogLevel/toText.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let Prelude =
        ../../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../../prelude.dhall

let toArguments =
      λ(options : Options.Type) →
          Prelude.List.map Text Text (λ(_ : Text) → "--file=${_}") options.files
        # optionalOptions
            Text
            (λ(_ : Text) → [ "--project-name=${_}" ])
            options.projectName
        # optionalOptions
            Text
            (λ(_ : Text) → [ "--project-directory=${_}" ])
            options.projectDirectory
        # optionalOptions
            Text
            (λ(_ : Text) → [ "--context=${_}" ])
            options.contextName
        # optionalOptions
            Text
            (λ(_ : Text) → [ "--env-file=${_}" ])
            options.environmentFile
        # (if options.noAnsi then [ "--no-ansi" ] else [] : List Text)
        # optionalOptions
            LogLevel.Type
            (λ(_ : LogLevel.Type) → [ "--log-level=${LogLevel.toText _}" ])
            options.logLevel

let test0 = assert : toArguments Options::{=} ≡ ([] : List Text)

let test1 =
        assert
      :   toArguments
            Options::{
            , files = [ "/some/docker-compose.yaml" ]
            , projectName = Some "project"
            , projectDirectory = Some "/path/to/project/directory"
            , contextName = Some "context"
            , environmentFile = Some "/path/to/file.env"
            , logLevel = Some LogLevel.Type.Debug
            , noAnsi = True
            , environment =
              [ { name = "CONFIG", value = "{\"json\": \"value\"}" } ]
            , workingDirectory = Some "/path/to/working/directory"
            }
        ≡ [ "--file=/some/docker-compose.yaml"
          , "--project-name=project"
          , "--project-directory=/path/to/project/directory"
          , "--context=context"
          , "--env-file=/path/to/file.env"
          , "--no-ansi"
          , "--log-level=DEBUG"
          ]

in  toArguments : Options.Type → List Text
