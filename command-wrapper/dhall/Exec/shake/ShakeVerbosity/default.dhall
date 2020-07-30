-- vim: filetype=dhall
--
-- Normally the default Shake verbosity is `Info`, see
-- <https://hackage.haskell.org/package/shake/docs/src/Development.Shake.Internal.Options.html#shakeOptions>,
-- however, that can be changed when constructing a custom Shake binary.

let ShakeVerbosity =
        ./Type.dhall sha256:85aa28a8ed92e7924805f4501411c185014af50a9f3227b27fdaa17e495a68af
      ? ./Type.dhall

in  ShakeVerbosity.Info
