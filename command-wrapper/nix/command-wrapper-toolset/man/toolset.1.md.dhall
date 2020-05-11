-- vim: filetype=dhall
--
-- Template for a toolset manual page.

let -- If toolset command is called for example `habit` then this is how
    -- `options` should be initialised:
    --
    -- ```Dhall
    -- Options::{
    -- , toolset =
    --     { upper = "HABIT"
    --     , name = "Habit"
    --     , command = "habit"
    --     , version = "1.0.0"
    --     }
    -- }
    -- ```
    --
    -- Field `name` should be initalised to author's name, if available.
    -- Finally `date` should be initalised to date of the last modification,
    -- not when the manual page was built.
    Options =
      { Type =
          { toolset :
              { upper : Text, name : Text, command : Text, version : Text }
          , name : Text
          , date : Text
          }
      , default = { name = "", date = "" }
      }

let -- Usage example for toolset named `habit`:
    --
    -- ```Dhall
    -- template
    --   Options::{
    --   , toolset =
    --       { upper = "HABIT"
    --       , name = "Habit"
    --       , command = "habit"
    --       , version = "1.0.0"
    --       }
    --   }
    -- ```
    template =
        λ(options : Options.Type)
      → ''
        % ${options.toolset.upper}(1) ${options.toolset.name} Toolset ${options.toolset.version} | ${options.toolset.name} Toolset
        % ${options.name}
        % ${options.date}
        
        
        # NAME
        
        `${options.toolset.command}` - TODO: Short description
        
        
        # USAGE
        ${options.toolset.command} \[GLOBAL\_OPTIONS] SUBCOMMAND \[\--] \[SUBCOMMAND\_ARGUMENTS]
        
        ${options.toolset.command} \[GLOBAL\_OPTIONS] help \[SUBCOMMAND]
        
        ${options.toolset.command} \[GLOBAL\_OPTIONS] config \[SUBCOMMAND] \[CONFIG\_OPTIONS]
        
        ${options.toolset.command} \[GLOBAL\_OPTIONS] completion \[COMPLETION\_OPTIONS]
        
        ${options.toolset.command} \[GLOBAL\_OPTIONS] version \[VERSION\_OPTIONS]
        
        ${options.toolset.command} {\--help|-h}
        
        ${options.toolset.command} {\--version|-V}
        
        
        # DESCRIPTION
        
        **TODO: Long description**
        
        
        # GLOBAL OPTIONS
        
        See section *GLOBAL OPTIONS* in `command-wrapper(1)`:
        
        ```Bash
        ${options.toolset.command} man command-wrapper
        ```
        
        
        # SUBCOMMAND
        
        To list all available subcommands along with short description run:
        
        ```Bash
        ${options.toolset.command} help --list
        ```
        
        Subcommands can be displayed in tree structure using `'.'` in the
        command name as a separator:
        
        ```Bash
        ${options.toolset.command} help --tree
        ```
        
        This allows us to organise subcommands in a hierarchical way.
        
        For more information on available run-time help see documentation of
        `help` subcommand on `command-wrapper-help(1)`:
        
        ```Bash
        ${options.toolset.command} man help
        ```
        
        We can see a lot aliases in the subcommand list.  These are either
        short hands for internal subcommands, or in case of
        `${options.toolset.command}`-specific commands they are usually aliases
        for:
        
        ```Bash
        ${options.toolset.command} exec EXEC_COMMAND
        ```
        
        To list those separately we can use:
        
        ```Bash
        ${options.toolset.command} exec {-l|--ls|--list}
        ```
        
        Once again they can be displayed in tree structure (using `'.'`
        character in the command name as a separator):
        
        ```Bash
        ${options.toolset.command} exec {-t|--tree}
        ```
        
        For each of those commands there’s an alias that allows them to be
        called as:
        
        ```Bash
        ${options.toolset.command} EXEC_COMMAND [ARGUMENT ...]
        ```
        
        For more information about `${options.toolset.command} exec` subcommand
        and its configuration see documentation in `command-wrapper-exec(1)`:
        
        ```Bash
        ${options.toolset.command} man exec
        ```
        
        
        # EXIT STATUS
        
        See section EXIT STATUS in `command-wrapper(1)`:
        
        ```Bash
        ${options.toolset.command} man command-wrapper
        ```
        
        
        # FILES AND DIRECTORIES
        
        Only `${options.toolset.command}`-specific stuff is documented here,
        for generic Command Wrapper toolset configuration stuff see also
        section *FILES AND DIRECTORIES* in `command-wrapper(1)`:
        
        ```Bash
        ${options.toolset.command} man command-wrapper
        ```
        
        `''${COMMAND_WRAPPER_USER_CONFIG_DIR:-''${XDG_CONFIG_HOME:-$HOME/.config}}/${options.toolset.command}/default.dhall`
        :   **TODO: Document configuration file**
        
            See also `COMMAND_WRAPPER_USER_CONFIG_DIR` and `XDG_CONFIG_HOME` in
            *ENVIRONMENT VARIABLES* section for more information on how Command
            Wrapper figures out where to look for this configuration file.
        
        
        # ENVIRONMENT VARIABLES
        
        Only `${options.toolset.command}`-specific stuff is documented here,
        for generic Command Wrapper toolset configuration stuff see also
        section *ENVIRONMENT VARIABLES* in `command-wrapper(1)`:
        
        ```Bash
        ${options.toolset.command} man command-wrapper
        ```
        
        
        # CONFIGURATION FILE
        
        See section *CONFIGURATION FILE* in `command-wrapper(1)`:
        
        ```Bash
        ${options.toolset.command} man command-wrapper
        ```
        
        As well as toolset configuration file documentation
        `command-wrapper-default.dhall(5)`:
        
        ```Bash
        ${options.toolset.command} man default.dhall
        ```
        
        
        # SEE ALSO
        
        command-wrapper(1), command-wrapper-help(1)
        
        * [XDG Base Directory Specification
          ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
        
        
        # BUGS
        
        Bugs specific to ${options.toolset.name} toolset: Fix them
        
        [Command Wrapper bug tracker
        ](https://github.com/trskop/command-wrapper/issues>
        ''

in  { Options, template }
