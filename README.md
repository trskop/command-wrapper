Description
===========

Some command line applications with a lot of commands try to avoid poluting
`$PATH` with all of them. One of the approaches to this is to have one top
level command exposed and the rest is implemented as subcommands. Subcommands
are either internal functions or external commands. Example of such application
is Git which uses mix of internal subcommands and external subcommand.

In general such toolset top level command has syntax like this:

    TOOLSET_COMMAND [GLOBAL_OPTIONS] SUBSYSTEM [SUBSYSTEM_OPTIONS]

This package provides universal top-level command, that can be named as
required, and API for subcommands. Subcommands may be written in any language,
they just need to be executable files that respect the subcommand API.
