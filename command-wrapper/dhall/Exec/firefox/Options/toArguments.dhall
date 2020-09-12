-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:054af9262f73048f90af26b55ff29e9d6cb0b4b7f8002bdb1994495636f4cc84
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:430f666c22a7337f0f016178b33a11479041b5bf168ed9779703b074d630eb99
          ? ./default.dhall
      }

let Profile =
      { Type =
            ../Profile/Type.dhall sha256:2fceab335c38877b2d53f69d40bb9be74c467b89cb7c80ac7960cf52ce8e82fa
          ? ../Profile/Type.dhall
      , toArguments =
            ../Profile/toArguments.dhall sha256:7d48376f1dd43c616a66c258e45316dc03305dd4ad09bb63f8898c7566b15bb9
          ? ../Profile/toArguments.dhall
      }

let Remote =
      { Type =
            ../Remote/Type.dhall sha256:f4db3526d25d60b7f0f09d1acc91534567990b5bab8514aaf64573b86648f10e
          ? ../Remote/Type.dhall
      , toArguments =
            ../Remote/toArguments.dhall sha256:96a14dfb5c9acff8a9c13e2109598b6bf6c8e897de4a31a26f687f598a62eda3
          ? ../Remote/toArguments.dhall
      }

let Open =
      { Type =
            ../Open/Type.dhall sha256:a63c2ded38e71c92968a0e34a80d6c2b5a9f7dd498864b4e69ef35b5e930fb5f
          ? ../Open/Type.dhall
      , toArguments =
            ../Open/toArguments.dhall sha256:0295987ff77f174d5935f3b501ec05c620513cd1973d64ca56fee62f1650005a
          ? ../Open/toArguments.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let Prelude =
        ../../../Prelude/package.dhall sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028
      ? ../../../Prelude/package.dhall

let toArguments =
      λ(options : Options.Type) →
          optionalOptions Profile.Type Profile.toArguments options.profile
        # optionalOptions Remote.Type Remote.toArguments options.remote
        # Prelude.List.concatMap Open.Type Text Open.toArguments options.open

let test0 = assert : toArguments Options::{=} ≡ ([] : List Text)

let test1 =
        assert
      :   toArguments
            Options::{
            , profile = Some (Profile.Type.Name "Default")
            , open = [ Open.Type.Url "https://example.com" ]
            }
        ≡ [ "-P", "Default", "https://example.com" ]

in  toArguments : Options.Type → List Text
