-- vim: filetype=dhall

{ bazel = { command = ./bazel/command, completion = ./bazel/completion }
, buildifier = { command = ./buildifier/command }
, completion =
    { optparse-applicative = ./completion/optparse-applicative
    , wordlist = ./completion/wordlist
    }
, direnv =
    { command = ./direnv/command
    , completion = ./direnv/completion
    , exec = ./direnv/exec
    }
, docker =
    { GlobalOptions = ./docker/DockerGlobalOptions
    , ExecOptions = ./docker/DockerExecOptions
    , RunOptions = ./docker/DockerRunOptions
    , globalOptions = ./docker/globalOptions
    , defaultGlobalOptions = ./docker/defaultGlobalOptions
    , envOptions = ./docker/envOptions
    , execOptions = ./docker/execOptions
    , defaultExecOptions = ./docker/defaultExecOptions
    , interactiveExecOptions = ./docker/interactiveExecOptions
    , runOptions = ./docker/runOptions
    , interactiveRunOptions = ./docker/interactiveRunOptions
    , ephemeralRunOptions = ./docker/ephemeralRunOptions
    , prune = ./docker/prune
    , exec = ./docker/exec
    , run = ./docker/run
    }
, docker-compose =
    { GlobalOptions = ./docker-compose/GlobalOptions
    , DownOptions = ./docker-compose/DownOptions
    , UpOptions = ./docker-compose/UpOptions
    , Action = ./docker-compose/Action
    , command = ./docker-compose/command
    , noAction = ./docker-compose/noAction
    , defaultGlobalOptions = ./docker-compose/defaultGlobalOptions
    , globalOptions = ./docker-compose/globalOptions
    , completion = ./docker-compose/completion
    }
, firefox =
    { Options = ./firefox/Options/Type
    , Profile = ./firefox/Profile/Type
    , Remote = ./firefox/Remote/Type
    , Open = ./firefox/Open/Type
    , command = ./firefox/command
    , defaultOptions = ./firefox/Options/default
    , options = ./firefox/Options/options
    }
, go-jira =
    { Options = ./go-jira/Options
    , command = ./go-jira/command
    , completion = ./go-jira/completion
    , options = ./go-jira/options
    }
, nix =
    { Command = ./nix/Command/Type
    , command = ./nix/command
    , completion = ./nix/completion
    , Command/show = ./nix/Command/show
    }
, pg_dump = { command = ./pg_dump/command }
, psql = { command = ./psql/command }
, run-mailcap = { command = ./run-mailcap/command }
, ssh =
    { ConnectTo = ./ssh/ConnectTo
    , DynamicForwardingOptions = ./ssh/DynamicForwardingOptions
    , Forwarding = ./ssh/Forwarding
    , ForwardingOptions = ./ssh/ForwardingOptions
    , ListenOn = ./ssh/ListenOn
    , Options = ./ssh/Options
    , command = ./ssh/command
    , defaultOptions = ./ssh/defaultOptions
    , options = ./ssh/options
    }
, stack = { command = ./stack/command, completion = ./stack/completion }
, tmux = { command = ./tmux/command }
, utils = { toShell = ./utils/to-shell }
, xdg-open = { command = ./xdg-open/command }
, yarn = { command = ./yarn/command, completion = ./yarn/completion }
}
