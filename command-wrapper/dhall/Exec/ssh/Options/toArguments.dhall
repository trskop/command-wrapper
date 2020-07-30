-- vim: filetype=dhall

let Options =
        ./Type.dhall sha256:5c7701e806aeb7526088ef25f1233f2237342a4b1fd617a028f1258983d5202c
      ? ./Type.dhall

let Forwarding =
      { Type =
            ../Forwarding/Type.dhall sha256:434b5e6be35d6dd97cd29665c8c8855be653a25396174de07ec3c158dd8e4cdb
          ? ../Forwarding/Type.dhall
      , toArguments =
            ../Forwarding/toArguments.dhall sha256:af2cf2ac82c79b95b7d5b5f9ff40a27dc87369b69e41c48b1414eb32662090d8
          ? ../Forwarding/toArguments.dhall
      }

let Prelude =
        ../../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../../prelude.dhall

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

in  λ(options : Options) →
        optionalOptions Text (λ(_ : Text) → [ "-i", _ ]) options.identityFile
      # Prelude.List.concatMap
          Forwarding.Type
          Text
          Forwarding.toArguments
          options.forwardings
      # optionalOptions Text (λ(_ : Text) → [ "-F", _ ]) options.configFile
      # optionalOptions
          Bool
          (λ(_ : Bool) → if _ then [ "-t" ] else [ "-T" ])
          options.allocatePseudoTerminal
      # (if options.doNotExecuteRemoteCommand then [ "-N" ] else [] : List Text)
      # (if options.preventReadingOfStdin then [ "-n" ] else [] : List Text)
