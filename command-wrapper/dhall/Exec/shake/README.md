# Shake

[Shake](https://shakebuild.com/) is build system implemented as a Haskell DSL.
That means that concrete instances of build systems built using Shake are
separate executables.  This fact affects how the smart constructor, introduced
here, behaves.

Shake homepage: [shakebuild.com](https://shakebuild.com/)

GitHub repo: [github.com/ndmitchell/shake](https://github.com/ndmitchell/shake)

Hackage: [hackage.haskell.org/package/shake](https://hackage.haskell.org/package/shake)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let build =
      CommandWrapper.ExecNamedCommand::{
      , name = "build"
      , description = Some "Run our Shake-based build system"
      , command =
          Exec.shake.command
            "/path/to/our/build/system"
            Exec.shake.Options::{
            , jobs = Some 0 -- Use jobs/threads based on the number of CPUs
            , printDirectory = Some True
            }
            CommandWrapper.Command.emptyArguments -- No extra arguments
            CommandWrapper.Environment.empty -- No extra environment variables
      }

in  build : CommandWrapper.ExecNamedCommand.Type
```
