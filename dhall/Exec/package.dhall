{ bazel =
    { command = ./bazel/command, completion = ./bazel/completion }
, buildifier =
    { command = ./buildifier/command }
, completion =
    { optparse-applicative =
        ./completion/optparse-applicative
    , wordlist =
        ./completion/wordlist
    }
, docker-compose =
    { command =
        ./docker-compose/command
    , noAction =
        ./docker-compose/noAction
    , defaultGlobalOptions =
        ./docker-compose/defaultGlobalOptions
    , globalOptions =
        ./docker-compose/globalOptions
    , completion =
        ./docker-compose/completion
    }
, docker =
    { globalOptions =
        ./docker/globalOptions
    , defaultGlobalOptions =
        ./docker/defaultGlobalOptions
    , envOptions =
        ./docker/envOptions
    , execOptions =
        ./docker/execOptions
    , defaultExecOptions =
        ./docker/defaultExecOptions
    , interactiveExecOptions =
        ./docker/interactiveExecOptions
    , runOptions =
        ./docker/runOptions
    , interactiveRunOptions =
        ./docker/interactiveRunOptions
    , ephemeralRunOptions =
        ./docker/ephemeralRunOptions
    , prune =
        ./docker/prune
    , exec =
        ./docker/exec
    , run =
        ./docker/run
    }
, yarn =
    { command = ./yarn/command, completion = ./yarn/completion }
, direnv =
    { command = ./direnv/command }
, firefox =
    { command = ./firefox/command }
, pg_dump =
    { command = ./pg_dump/command }
, psql =
    { command = ./psql/command }
, run-mailcap =
    { command = ./run-mailcap/command }
, ssh =
    { command =
        ./ssh/command
    , defaultOptions =
        ./ssh/defaultOptions
    , options =
        ./ssh/options
    }
, stack =
    { command = ./stack/command, completion = ./stack/completion }
, tmux =
    { command = ./tmux/command }
, xdg-open =
    { command = ./xdg-open/command }
}
