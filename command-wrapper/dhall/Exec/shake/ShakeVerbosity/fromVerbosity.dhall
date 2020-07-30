-- vim: filetype=dhall
--
-- Dhall representation of
-- <https://hackage.haskell.org/package/shake/docs/Development-Shake.html#t:Verbosity>

let ShakeVerbosity =
        ./Type.dhall sha256:85aa28a8ed92e7924805f4501411c185014af50a9f3227b27fdaa17e495a68af
      ? ./Type.dhall

let Verbosity =
        ../../../CommandWrapper/Verbosity/package.dhall sha256:488f95a5a27b82653c5a759b592b08e16940d1698dcf956fcbd9c153cb2547f2
      ? ../../../CommandWrapper/Verbosity/package.dhall

let fromVerbosity =
      λ(_ : Verbosity.Type) →
        Verbosity.fold
          ShakeVerbosity
          { Annoying = ShakeVerbosity.Diagnostic
          , Normal = ShakeVerbosity.Info
          , Silent = ShakeVerbosity.Silent
          , Verbose = ShakeVerbosity.Verbose
          }
          _

in  fromVerbosity : Verbosity.Type → ShakeVerbosity
