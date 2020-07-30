-- vim: filetype=dhall

let DownOptions =
      { Type =
            ./Type.dhall sha256:760e4cd0b9b14aa5c3225f155da55eb8eef9c41a75a4535665ac5d7c6aceb182
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:3ac386ca465801038a423b857211761ad708aacaefe514b0d9824e4d819f2633
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let emptyArguments = [] : List Text

let toArguments =
      λ(options : DownOptions.Type) →
          optionalOptions
            < All | Local >
            ( λ(_ : < All | Local >) →
                [ "--rmi=" ++ merge { All = "all", Local = "local" } _ ]
            )
            options.removeImages
        # ( if    options.removeNamedVolumes
            then  [ "--volumes" ]
            else  emptyArguments
          )
        # ( if    options.removeOrphanContainers
            then  [ "--remove-orphans" ]
            else  emptyArguments
          )

let test0 = assert : toArguments DownOptions::{=} ≡ emptyArguments

let test1 =
        assert
      :   toArguments
            DownOptions::{
            , removeImages = Some < All | Local >.All
            , removeNamedVolumes = True
            , removeOrphanContainers = True
            }
        ≡ [ "--rmi=all", "--volumes", "--remove-orphans" ]

in  toArguments : DownOptions.Type → List Text
