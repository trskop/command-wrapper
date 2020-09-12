-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:e908cbddb6ec0b95462e513b70eaf6181aed7908212ad1f5614ee736bc174c9c
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:ce42936b737e0d7ccd23c499b1484179bd083c5a4f31749ab46aed2402cd1adc
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let Prelude =
        ../../../Prelude/package.dhall sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028
      ? ../../../Prelude/package.dhall

let noArguments = [] : List Text

let toArguments =
      λ(_ : Options.Type) →
          ( if    Prelude.Natural.isZero _.lines
            then  noArguments
            else  if Prelude.Natural.greaterThanEqual _.lines 3
            then  [ "--lines=${Prelude.Natural.show _.lines}" ]
            else  [ "--lines=3" ]
          )
        # optionalOptions Text (λ(t : Text) → [ "--prompt=${t}" ]) _.prompt
        # (if _.showScores then [ "--show-scores" ] else noArguments)
        # optionalOptions Text (λ(path : Text) → [ "--tty=${path}" ]) _.tty
        # optionalOptions Text (λ(s : Text) → [ "--query=${s}" ]) _.query
        # optionalOptions
            Text
            (λ(s : Text) → [ "--show-matches=${s}" ])
            _.showMatches
        # ( if    Prelude.Natural.isZero _.workers
            then  noArguments
            else  [ "--workers=${Prelude.Natural.show _.workers}" ]
          )

let noArgumentsByDefault = assert : toArguments Options.default ≡ noArguments

in  toArguments : Options.Type → List Text
