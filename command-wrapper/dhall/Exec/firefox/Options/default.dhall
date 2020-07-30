-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:054af9262f73048f90af26b55ff29e9d6cb0b4b7f8002bdb1994495636f4cc84
          ? ./Type.dhall
      }

let Profile =
        ../Profile/Type.dhall sha256:2fceab335c38877b2d53f69d40bb9be74c467b89cb7c80ac7960cf52ce8e82fa
      ? ../Profile/Type.dhall

let Remote =
        ../Remote/Type.dhall sha256:f4db3526d25d60b7f0f09d1acc91534567990b5bab8514aaf64573b86648f10e
      ? ../Remote/Type.dhall

let Open =
        ../Open/Type.dhall sha256:a63c2ded38e71c92968a0e34a80d6c2b5a9f7dd498864b4e69ef35b5e930fb5f
      ? ../Open/Type.dhall

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default =
        CommonOptions.default
      ∧ { profile = None Profile, remote = None Remote, open = [] : List Open }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
