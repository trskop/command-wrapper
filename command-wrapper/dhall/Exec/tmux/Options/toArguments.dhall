-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:b583193040d153654a70a31291aa959845a9d3246c367e65567b3d96dd3c6730
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:c194361ef28684b8a55c18d6e9176f2b7c77878cb9f52a94cc568b70f9695f55
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let noArguments = [] : List Text

let toArguments =
      λ(opts : Options.Type) →
        optionalOptions Text (λ(file : Text) → [ "-f", file ]) opts.config

let test0 = assert : toArguments Options::{=} ≡ noArguments

let test1 =
        assert
      :   toArguments
            Options::{
            , term = Some "xterm-256color"
            , config = Some "/home/user/.config/tmux/tmux.conf"
            }
        ≡ [ "-f", "/home/user/.config/tmux/tmux.conf" ]

in  toArguments : Options.Type → List Text
