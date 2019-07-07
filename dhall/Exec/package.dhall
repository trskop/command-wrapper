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
    }
, docker =
    { command = ./docker/command }
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
