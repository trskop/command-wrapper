-- vim: filetype=dhall

let OutputType =
        ./Type.dhall sha256:070309fd254a430683ba3700b29075cdf60c688fc5a82e0a4ab62c2605cd815d
      ? ./Type.dhall

let toArguments =
      λ(_ : OutputType) →
        merge { Raw = [ "--raw-output" ], Join = [ "--join-output" ] } _

let test0 = assert : toArguments OutputType.Raw ≡ [ "--raw-output" ]

in  toArguments : OutputType → List Text
