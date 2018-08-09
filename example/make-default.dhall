  λ(prefix : Text)
→ { aliases = [] : List { alias : Text, command : Text, arguments : List Text }
  , searchPath = ["/home/peter/.local/lib/${prefix}"]
  , extraHelpMessage = [] : Optional Text
  }
