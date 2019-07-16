{ bazel =
    { command = ./bazel/command }
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
    { command = ./yarn/command }
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
, stack =
    { command = ./stack/command }
, tmux =
    { command = ./tmux/command }
, xdg-open =
    { command = ./xdg-open/command }
}