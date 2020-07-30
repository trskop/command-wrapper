-- vim: filetype=dhall

let Profile =
        ./Type.dhall sha256:2fceab335c38877b2d53f69d40bb9be74c467b89cb7c80ac7960cf52ce8e82fa
      ? ./Type.dhall

let toArguments =
      λ(_ : Profile) →
        merge
          { Name = λ(name : Text) → [ "-P", name ]
          , Path = λ(path : Text) → [ "--profile", path ]
          , Manager = [ "--ProfileManager" ]
          }
          _

let test0 = assert : toArguments (Profile.Name "Default") ≡ [ "-P", "Default" ]

let test1 = assert : toArguments Profile.Manager ≡ [ "--ProfileManager" ]

in  toArguments : Profile → List Text
