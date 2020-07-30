-- vim: filetype=dhall

let CommonOptions =
        ../../CommonOptions/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ../../CommonOptions/Type.dhall

let Profile =
        ../Profile/Type.dhall sha256:2fceab335c38877b2d53f69d40bb9be74c467b89cb7c80ac7960cf52ce8e82fa
      ? ../Profile/Type.dhall

let Remote =
        ../Remote/Type.dhall sha256:f4db3526d25d60b7f0f09d1acc91534567990b5bab8514aaf64573b86648f10e
      ? ../Remote/Type.dhall

let Open =
        ../Open/Type.dhall sha256:a63c2ded38e71c92968a0e34a80d6c2b5a9f7dd498864b4e69ef35b5e930fb5f
      ? ../Open/Type.dhall

in    CommonOptions
    ⩓ { profile : Optional Profile, remote : Optional Remote, open : List Open }
