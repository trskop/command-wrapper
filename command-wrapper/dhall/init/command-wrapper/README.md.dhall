-- vim: filetype=dhall

λ(library : { commandWrapper : Text, exec : Text }) →
λ(runtimeDirectory : { libDir : Text, manDir : Text }) →
  ''
  # Command Wrapper configuration

  Tool for creating customised command-line toolsets.  This directory contains
  its top-level configuration


  ## Documentation

  Offline documentation is provided in the form of manual pages.  Best starting
  point is `command-wrapper(1)`.

  Online documentation is available on [github.com/trskop/command-wrapper
  ](https://github.com/trskop/command-wrapper).
  ''
