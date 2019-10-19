-- vim: filetype=dhall
--
-- ```
-- $ TOOLSET exec [--notify] --expession=$REPO/dhall/example/exec/echo Hello World
-- Hello World
-- ```

  λ(verbosity : < Annoying | Normal | Silent | Verbose >)
→ λ(colourOutput : < Always | Auto | Never >)
→ λ(arguments : List Text)
→ { arguments = arguments
  , command = "echo"
  , environment = [] : List { name : Text, value : Text }
  , searchPath = True
  , workingDirectory = None Text
  }
