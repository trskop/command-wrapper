-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:d10faab8c8388e68d0e931248dee4741cbcef8b593095d8dd8a42a54c2272ab5
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:6bcdf525021173128fe71180d9f1a2e99cf514d0ab07bf391693d107e846125f
          ? ./default.dhall
      }

let Action =
      { Type =
            ../Action/Type.dhall sha256:8df4bf8c0bcc06dae0cc7235ab731ddf210b072ea8ece9ff96b8abc5d138f36b
          ? ../Action/Type.dhall
      , toArguments =
            ../Action/toArguments.dhall sha256:5ff2f8297b445bbf401f9417f11be30c792e99345314f35344d325e52106fe2d
          ? ../Action/toArguments.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let noArguments = [] : List Text

let toArguments =
      λ(opts : Options.Type) →
          (if opts.silent then [ "-s" ] else noArguments)
        # (if opts.nostart then [ "--nostart" ] else noArguments)
        # (if opts.diffMode then [ "-d" ] else noArguments)
        # (if opts.previousWindow then [ "-l" ] else noArguments)
        # optionalOptions
            Text
            (λ(cmd : Text) → [ "-cc", cmd ])
            opts.beforeCommand
        # optionalOptions Text (λ(cmd : Text) → [ "-c", cmd ]) opts.afterCommand
        # optionalOptions
            Text
            (λ(name : Text) → [ "--servername", name ])
            opts.serverName
        # optionalOptions Action.Type Action.toArguments opts.action

let test0 =
        assert
      :   toArguments
            Options::{
            , silent = True
            , nostart = True
            , action = Some
                ( Action.Type.Split
                    { vertical = True, files = [ "./foo", "./bar" ] }
                )
            }
        ≡ [ "-s", "--nostart", "-O", "./foo", "./bar" ]

in  toArguments : Options.Type → List Text
