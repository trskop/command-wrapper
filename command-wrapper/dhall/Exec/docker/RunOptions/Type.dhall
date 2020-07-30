-- vim: filetype=dhall

let ExecOptions =
        ../ExecOptions/Type.dhall sha256:f060f3aa27210e24ea2ff0fd222b3c0d4c376f79077d376c3b40596ad7d3d74a
      ? ../ExecOptions/Type.dhall

let RunOptions = ExecOptions ⩓ { remove : Bool }

in  RunOptions : Type
