% COMMAND-WRAPPER-DEFAULT.DHALL(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 8th April 2020


# NAME

`default.dhall` - Command Wrapper toolset configuration file.


# DESCRIPTION

Command Wrapper toolset configuration files (`default.dhall`) are written in
Dhall configuration language.  The whole file is one Dhall expression of the
type (anything starting with `CommandWrapper` is part of Command Wrapper Dhall
library):

```Dhall
-- Subcommand aliases.  These can be used to invoke subcommand in
-- the form:
--
--     TOOLSET [GLOBAL_OPTIONS] ALIAS [EXTRA_ARGUMENTS]
--
-- Which is then translated into:
--
--     TOOLSET [GLOBAL_OPTIONS] COMMAND [ARGUMENTS] [EXTRA_ARGUMENTS]
--
-- See also `command-wrapper(1)` manual page for more information about command
-- onvocation.
--
-- In addition to the above `help` also understands aliases:
--
--     TOOLSET [GLOBAL_OPTIONS] help ALIAS
--
-- Is the same as:
--
--     TOOLSET [GLOBAL_OPTIONS] help COMMAND
--
-- To see the description of individual aliases we can use:
--
--     TOOLSET [GLOBAL_OPTIONS] help --aliases
--
-- For more information see `command-wrapper-help(1)` manual page.
--
-- `SubcommandAlias` type is a type of a record with following fields:
--
--     -- Name of the alias, i.e. name under which we can execute
--     -- this subcommand.
--     { alias : Text
--
--     -- Optional alias description which serves as a short help message.
--     , description : Optional Text
--
--     -- Command Wrapper subcommand to be executed under the
--     -- name specified in `alias` field.
--     , command : Text
--
--     -- Arguments premended to the argument list when invoked.
--     , arguments : List Text
--     }
{ aliases : List SubcommandAlias

-- Path where to search for subcommands.  Definition from
--
--     ${XDG_CONFIG_HOME:-${HOME}/.config}/command-wrapper/default.dhall
--
-- are concatenated with those from
--
--     ${XDG_CONFIG_HOME:-${HOME}/.config}/${toolset}/default.dhall
, searchPath : List Text

-- Path where to search for manual pages.  Definition from
--
--     ${XDG_CONFIG_HOME:-${HOME}/.config}/command-wrapper/default.dhall
--
-- are concatenated with those from
--
--     ${XDG_CONFIG_HOME:-${HOME}/.config}/${toolset}/default.dhall
, manPath : List Text

-- Description of the toolset command printed as part of help message.
, description : Optional Text

-- Allows user to override default command wrapper behaviour when
-- it comes colourised output.  By default Command Wrapper uses
-- the value:
--
--     CommandWrapper.ColourOutput.Type.Auto
--
-- Unless `NO_COLOR` environment variable is set, in which case
-- following is used:
--
--     CommandWrapper.ColourOutput.Type.Never
--
-- See also `--colour` option and `NO_COLOR` environment variable
-- descriptions on `command-wrapper(1)` manual page.
, colourOutput : Optional CommandWrapper.ColourOutput.Type

-- Extra text to be displayed when `TOOLSET help` or `TOOLSET --help`
-- is invoked.  It is useful for providing important examples, and
-- references to additional documentation.
, extraHelpMessage : Optional Text

-- Default verbosity to be used when command is invoked.  See
-- `--verbosity` option on `command-wrapper(1)` manual page for more details.
, verbosity : Optional CommandWrapper.Verbosity.Type
}
```

Imports in the above description aren't necessary.  To get fully
normalised/desugared version just paste it as an input to `dhall` command.

It is highly advised  to use imports and to use smart constructor syntax
instead of listing all values.  It makes it easier to migrate configuration
between versions.  Smart constructor syntax example:

```Dhall
let -- To obtain stable and reliable Command Wrapper library import use:
    --
    -- ```
    -- TOOLSET completion --library --dhall=command-wrapper --import
    -- ```
    --
    -- For more information see `command-wrapper-completion(1)` manual page.
    --
    -- Import provided here is to the latest version on GitHub, which may not
    -- be what you want.  Best practice is to create a separate file for this
    -- import and use that instead.  Upgrading to newever version becomes very
    -- easy afterwards.
    --
    -- Note that adding a hash will allow Dhall to cache the import.
    CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

in  CommandWrapper.ToolsetConfig::{
    , aliases = ./aliases-common.dhall # ./exec-aliases.dhall

    , searchPath =
        CommandWrapper.ToolsetConfig.defaultSearchPath
        env:HOME as Text
        "command-wrapper"
    }
```


# FILES

Toolset configuration files are read from these locations (in specified order):

```
${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/command-wrapper/default.dhall
${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/command-wrapper/default.dhall
${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/default.dhall
${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/${toolset}/default.dhall
```

Above list uses Bash syntax for environment variable alternative:

```Bash
${SOME_ENV_VAR:-default value of SOME_ENV_VAR is not set or is empty}
```

In other words, the value of:

```Bash
${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}
```

Expands into the value of:

*   `COMMAND_WRAPPER_USER_CONFIG_DIR` if set and non-empty,
*   or the value of `XDG_CONFIG_HOME` if set and non-empty,
*   or the value `${HOME}/.config`.

After Command Wrapper toolset finds all the configuration files it then
composes them together to form one configuration.  The way the fields are
composed depends on the individual fields (see *DESCRIPTION* section).  Usually
they are concatenated, or in some case specific toolset configuration overrides
`command-wrapper` configuration, i.e. configuration files mentioned later in
the above list override those that are specified before it.


# SPEEDING UP LOADING USING DHALL CACHE

If `default.dhall` configuration files become big (usually due to a lot of
aliases) then we can speed up load time by using Dhall caching mechanism.

Dhall interpreter caches imports and Dhall expressions to speed up loading.
This feature can be leveraged only if we provide semantic consistency hash to
our expression. To do so we need to use imports instead of directly putting the
desired configuration into `default.dhall`.  As a best practice we propose
using this as a content of `default.dhall` (just an example, your semantic
hash will most certainly differ):

```Dhall
  ./default/constructor.dhall sha256:ab194fe375a7d7f20d62090cd2395b30c6eb3d4667bffe0ea383e9dd441e6248
? ./default/constructor.dhall
```

The above allows Dhall interpreter to lookup already cached expression using
provided semantic hash, if it fails it will follow the import.  Reason for
using alternative syntax (`?`) is that if the hash doesn't match the import it
will still proceed.  Be aware that if there already is a cached value with that
hash, and configuration file has changed, then Dhall interpreter will load the
old value.

Example above implies following directory structure for toolset configuration
files:

```
~/.config/  <-- See `XDG_CONFIG_HOME` and `COMMAND_WRAPPER_USER_CONFIG_DIR`
│               environment variables on how to use a different directory
│               for configuration files.
│
├── command-wrapper/  <-- Global configuration shared by all toolsets.
│   │
│   ├── default.dhall  <-- Imports `./default/constructor.dhall` protected by
│   │                      semantic hash.
│   ├── default/
│   │   ├── constructor.dhall
│   │   └── ...  <-- Other configuration files used by `constructor.dhall`.
│   └── ...
│
└── ${toolset}/  <-- Configuration for only ${toolset}.
    │
    ├── default.dhall  <-- Imports `./default/constructor.dhall` protected by
    │                      semantic hash.
    ├── default/
    │   ├── constructor.dhall
    │   └── ...  <-- Other configuration files used by `constructor.dhall`.
    └── ...
```

Advantage of this directory structure is that when the configuration becomes
big it can easily be split into multiple files.

We can generate that content using (where the value of `CONFIG_DIRECTORY` has
to be substituted):

```Bash
TOOLSET --change-directory="${CONFIG_DIRECTORY}" \
    config --dhall-freeze --for-caching --no-remote-only \
    --expression='./default/constructor.dhall' \
    --output="${CONFIG_DIRECTORY}/default.dhall"
```

For more information see `command-wrapper-config(1)` manual page.


# DEBUGGING CONFIGURATION

Command Wrapper's `config` subcommand provides `--get` option which prints out
effective configuration.  What we mean by effective configuration is a superset
of what is in `default.dhall` configuration files, and it includes values
passed using environment variables and global options.  This is how we can get
the effective configuration:

```Bash
TOOLSET config --get
```


# SEE ALSO

command-wrapper(1), command-wrapper-completion(1), command-wrapper-config(1),
command-wrapper-help(1)

*   [Dhall configuration language](https://dhall-lang.org)
*   [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
