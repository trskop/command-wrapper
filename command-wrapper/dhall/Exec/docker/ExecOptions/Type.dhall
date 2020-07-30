-- vim: filetype=dhall

let Environment =
        ../Environment/Type.dhall sha256:bfc2cb080bb0cac5a42a81beb707437389fa9c6f8e54ae8dd7ce09c6566140ee
      ? ../Environment/Type.dhall

let DockerExecOptions =
      { interactive : Bool
      , allocateTty : Bool
      , detach : Bool
      , user : Optional Text
      , workingDirectory : Optional Text
      , environment : Environment
      , environmentFile : Optional Text
      }

in  DockerExecOptions : Type
