-- vim: filetype=dhall

let ShakeVerbosity =
        ./Type.dhall sha256:85aa28a8ed92e7924805f4501411c185014af50a9f3227b27fdaa17e495a68af
      ? ./Type.dhall

let Prelude =
        ../../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../../prelude.dhall

let toNatural =
      λ(_ : ShakeVerbosity) →
        merge
          { Silent = 0
          , Error = 1
          , Warn = 2
          , Info = 3
          , Verbose = 4
          , Diagnostic = 5
          }
          _

let -- Function that figures out how to make the transition from current onto
    -- desired verbosity levels.
    --
    -- The logic behind `--quiet` and `--verbose` flags is:
    --
    -- * `--quiet` -- decrease one level
    -- * `--verbose` -- increase one level
    delta =
      λ(current : ShakeVerbosity) →
      λ(desired : ShakeVerbosity) →
        let currentNum = toNatural current
        
        let desiredNum = toNatural desired
        
        in  if    Prelude.Natural.lessThanEqual currentNum desiredNum
            then  Prelude.List.replicate
                    (Prelude.Natural.subtract currentNum desiredNum)
                    Text
                    "--verbose"
            else  Prelude.List.replicate
                    (Prelude.Natural.subtract desiredNum currentNum)
                    Text
                    "--quiet"

let testDelta0 =
      assert : delta ShakeVerbosity.Info ShakeVerbosity.Info ≡ ([] : List Text)

let testDelta1 =
        assert
      :   delta ShakeVerbosity.Info ShakeVerbosity.Diagnostic
        ≡ [ "--verbose", "--verbose" ]

let testDelta1 =
        assert
      :   delta ShakeVerbosity.Info ShakeVerbosity.Silent
        ≡ [ "--quiet", "--quiet", "--quiet" ]

let -- Shake command line API doesn't make it easy to set a specific value in
    -- all cases and that's why we need to ask for the default value. Normally
    -- the default Shake verbosity is `Info`, see
    -- <https://hackage.haskell.org/package/shake/docs/src/Development.Shake.Internal.Options.html#shakeOptions>,
    -- however, that can be changed when constructing a custom Shake binary.
    toArguments =
      λ(default : ShakeVerbosity) →
      λ(desired : ShakeVerbosity) →
        merge
          { Silent = [ "--silent" ]
          , Error = delta default ShakeVerbosity.Error
          , Warn = delta default ShakeVerbosity.Warn
          , Info = delta default ShakeVerbosity.Info
          , Verbose = delta default ShakeVerbosity.Verbose
          , Diagnostic = [ "--debug" ]
          }
          desired

in    toArguments
    : ∀(default : ShakeVerbosity) → ∀(desired : ShakeVerbosity) → List Text
