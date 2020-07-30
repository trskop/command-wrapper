-- vim: filetype=dhall
--
-- Partial represenation of Shake command line options.

let ShakeVerbosity =
        ../ShakeVerbosity/Type.dhall sha256:85aa28a8ed92e7924805f4501411c185014af50a9f3227b27fdaa17e495a68af
      ? ../ShakeVerbosity/Type.dhall

let CompactOutput =
        ../CompactOutput/Type.dhall sha256:3f7e82ca4628384d6df93a0c6b8813d1d38a438b096695cb219c24ffae9cc87b
      ? ../CompactOutput/Type.dhall

let Environment =
        ../../../CommandWrapper/Environment/Type sha256:bfc2cb080bb0cac5a42a81beb707437389fa9c6f8e54ae8dd7ce09c6566140ee
      ? ../../../CommandWrapper/Environment/Type

in  -- Don't build anything if set to `True`.
    { noBuild : Bool

    -- Change to DIRECTORY before doing anything.
    , directory : Optional Text

    -- Colorize the output.
    , colour : Bool

    -- Use a compact Bazel/Buck style output.
    , compactOutput : Optional CompactOutput

    -- Allow specified number of jobs/threads at once. When `Some 0` the default
    -- value (number of CPUs) is used.
    , jobs : Optional Natural

    -- Keep going when some targets can't be made.
    , keepGoing : Optional Bool

    -- Try and avoid running external programs.
    , skipCommands : Bool

    -- Set verbosity level to desired value. See `../ShakeVerbosity/Type` for
    -- more information.
    , verbosity : Optional ShakeVerbosity

    -- Print the current directory.
    , printDirectory : Optional Bool

    -- Don't print build time.
    , noTime : Bool

    -- Print phase timings.
    , timings : Bool

    -- This doesn't populate any Shake command line options, but it is passed
    -- to 'CommandWrapper.ExecCommand' constructor.
    , environment : Environment
    }
