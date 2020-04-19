-- vim: filetype=dhall
--
-- Template for Command Wrapper subcommand manual page.

let Options =
      { Type =
          { toolset :
              { upper : Text
              , name : Text
              , manpage : Text
              , command : Text
              , version : Text
              }
          , subcommand : { upper : Text, command : Text, exe : Text }
          , name : Text
          , date : Text
          }
      , default = {=}
      }

let template =
        λ(options : Options.Type)
      → ''
        % ${options.toolset.upper}-${options.subcommand.upper}(1) ${options.toolset.name} Toolset ${options.toolset.version} | ${options.toolset.name} Toolset
        % ${options.name}
        % ${options.date}
        
        
        # NAME
        
        `${options.subcommand.exe}` - TODO: Short description
        
        
        # USAGE
        
        ${options.toolset.command} \[GLOBAL\_OPTIONS] ${options.subcommand.command} \[OPTIONS]
        
        ${options.toolset.command} \[GLOBAL\_OPTIONS] ${options.subcommand.command} {\--help|-h}
        
        ${options.toolset.command} \[GLOBAL\_OPTIONS] help \[\--man] ${options.subcommand.command}
        
        
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
        
        `''${COMMAND_WRAPPER_USER_CONFIG_DIR:-''${XDG_CONFIG_HOME:-$HOME/.config}}/${options.subcommand.command}/${options.subcommand.command}.dhall`
        :   **TODO: Document configuration file**
        
            See also `COMMAND_WRAPPER_USER_CONFIG_DIR` and `XDG_CONFIG_HOME` in
            *ENVIRONMENT VARIABLES* section for more information on how Command
            Wrapper figures out where to look for this configuration file.
        
        
        # ENVIRONMENT VARIABLES
        
        See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
        mentioned there applies to this subcommand as well.
        
        `COMMAND_WRAPPER_USER_CONFIG_DIR`, `XDG_CONFIG_HOME`
        :   Overrides where this subcommand expects its configuration file.  More
            in-depth description as well as how configuration files are looked up can
            be found in `command-wrapper(1)` manual page, which can be viewed by
            calling:
        
            ```Bash
            ${options.subcommand.command} help --man command-wrapper
            ```
        
        
        # EXAMPLES
        
        **TODO: Provide few usage examples**
        
        
        # SEE ALSO
        
        ${options.toolset.manpage}(1), command-wrapper(1), command-wrapper-help(1)
        
        * [XDG Base Directory Specification
          ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
        
        
        # BUGS
        
        Fix them.
        ''

in  { Options, template }
