-- vim: filetype=dhall

let CommonOptions =
        ../../CommonOptions/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ../../CommonOptions/Type.dhall

let Action =
        ../Action/Type.dhall sha256:8df4bf8c0bcc06dae0cc7235ab731ddf210b072ea8ece9ff96b8abc5d138f36b
      ? ../Action/Type.dhall

let NvrOptions =
      { silent : Bool
      , nostart : Bool
      , beforeCommand : Optional Text
      , afterCommand : Optional Text
      , diffMode : Bool
      , previousWindow : Bool
      , serverName : Optional Text
      , action : Optional Action
      }

in  CommonOptions ⩓ NvrOptions
