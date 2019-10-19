-- vim: filetype=dhall
--
-- Useful to test desktop notifications for commands that fail.
--
-- ```
-- $ TOOLSET exec --notify --expession=$REPO/dhall/example/exec/false
-- Hello World
-- ```

  λ(verbosity : < Annoying | Normal | Silent | Verbose >)
→ λ(colourOutput : < Always | Auto | Never >)
→ λ(arguments : List Text)
→ { arguments = [] : List Text
  , command = "false"
  , environment = [] : List { name : Text, value : Text }
  , searchPath = True
  , workingDirectory = None Text
  }
