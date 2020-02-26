# youtube-dl

youtube-dl is a command-line program to download videos from YouTube and a few
more sites.  [ytdl-org.github.io/youtube-dl/
](https://ytdl-org.github.io/youtube-dl/)

Repository: [github.com/ytdl-org/youtube-dl/
](https://github.com/ytdl-org/youtube-dl/)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/Exec/package.dhall

let emptyArguments = CommandWrapper.Command.emptyArguments

let Environment/empty = CommandWrapper.Environment.empty

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let youtubeDownloadDir =
      Some (env:XDG_DOWNLOAD_DIR as Text ? "${env:HOME as Text}/Downloads")

let youtube-dl = CommandWrapper.ExecNamedCommand::{
      , name = "youtube-dl"
      , description =
          Some (
              "Just invoke `youtube-dl`, but in Downloads directory and with"
          ++  " command line completion."
          )
      , command =
          Exec.youtube-dl.command
            youtubeDownloadDir
            CommandWrapper.Command.emptyArguments
      , completion =
          Some
            ( Exec.youtube-dl.completion
                toolset
                youtubeDownloadDir
                CommandWrapper.Command.emptyArguments
            )
      }

in  youtube-dl
```
