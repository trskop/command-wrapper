-- vim: filetype=dhall
--
-- Template for Command Wrapper subcommand manual page.

  λ ( toolset
    : { upper : Text
      , name : Text
      , manpage : Text
      , command : Text
      , version : Text
      }
    )
→ λ(subcommand : { upper : Text, command : Text, exe : Text })
→ λ(name : Text)
→ λ(date : Text)
→ ''
  % ${toolset.upper}-${subcommand.upper}(1) ${toolset.name} Toolset ${toolset.version} | ${toolset.name} Toolset
  % ${name}
  % ${date}
  
  
  # NAME
  
  `${subcommand.exe}` -- **TODO: Short description**
  
  
  # USAGE
  
  ${toolset.command} \[GLOBAL\_OPTIONS] ${subcommand.command} \[OPTIONS]
  
  ${toolset.command} \[GLOBAL\_OPTIONS] ${subcommand.command} {\--help|-h}
  
  ${toolset.command} \[GLOBAL\_OPTIONS] help \[\--man] ${subcommand.command}
  
  
  # DESCRIPTION
  
  **TODO: Long description**
  
  
  # OPTIONS
  
  For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.
  
  **TODO: Document options**
  
  \--help, -h
  :   Print short help message and exit.  Same as: `yx help jmp`.
  
  
  # EXIT STATUS
  
  For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
  manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
  subcommand will be listed below.
  
  **TODO: Document exit codes**
  
  
  # FILES
  
  `''${XDG_CONFIG_HOME:-$HOME/.config}/${subcommand.command}/${subcommand.command}.dhall`
  :   **TODO: Document configuration file**
  
      See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section for more
      information on how Command Wrapper figures out where to look for this
      configuration file.
  
  
  # ENVIRONMENT VARIABLES
  
  See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
  mentioned there applies to this subcommand as well.
  
  `XDG_CONFIG_HOME`
  :   Overrides where this subcommand expects its configuration file.  It follows
      this simple logic:
  
      * If `XDG_CONFIG_HOME` environment variable is set then the configuration
        file has path:
  
          ```
          ''${XDG_CONFIG_HOME}/${subcommand.command}/${subcommand.command}.dhall
          ```
  
      * If `XDG_CONFIG_HOME` environment variable is not set then default value
        is used instead:
  
          ```
          ''${HOME}/.config/${subcommand.command}/${subcommand.command}.dhall
          ```
  
      See [XDG Base Directory Specification
      ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
      for more information on rationale behind this.
  
  
  # EXAMPLES
  
  **TODO: Provide few usage examples**
  
  
  # SEE ALSO
  
  ${toolset.manpage}(1), command-wrapper(1)
  
  * [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
  
  
  # BUGS
  
  Fix them.
  ''
