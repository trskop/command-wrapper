-- vim: filetype=dhall

{ completionFor : Text
-- ^ Name of the command for which the completion is intended.  It has to be
-- the same name as one would use to invoke the command on command line.
, completion : Optional Text
-- ^ Bash completion script to use. If this is not provided then
-- `searchForCompletionFile` should be set to `True`, otherwise there would be
-- nothing to execute.
, entryPoint : Text
-- ^ Bash completion is usually a function that is then hooked into the shell.
-- This value tells our script what's the name of that function. Without it we
-- would not be able to invoke it as the name is completely arbitrary.
, searchForCompletionFile : Bool
-- ^ If set to `True` then the script will search for Bash completion script in
-- standard system and Nix locations.  If `completion` script is provided as
-- well then we first try to locate the completion script somewhere on the
-- system and if it fails then `completion` script is executed.
, strictMode : Bool
-- ^ Set Bash strict mode.  By default we do not, since a lot of Bash
-- completion scripts do not work with it being set.
, sourceLibrary : Optional Text
-- ^ Source this library before executing any Bash completion script.  Some
-- completions depend on shell libraries being sourced, most notable one is the
-- `/usr/share/bash-completion/bash_completion` library.
}
