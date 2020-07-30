-- vim: filetype=dhall

let Remote =
        ./Type.dhall sha256:f4db3526d25d60b7f0f09d1acc91534567990b5bab8514aaf64573b86648f10e
      ? ./Type.dhall

let toArguments =
      λ(_ : Remote) →
        merge
          { NoRemote = [ "--no-remote" ], NewInstance = [ "--new-instance" ] }
          _

let test0 = assert : toArguments Remote.NoRemote ≡ [ "--no-remote" ]

in  toArguments : Remote → List Text
