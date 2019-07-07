{ bazel =
    ./bazel/command
, completion =
    { optparse-applicative =
        ./completion/optparse-applicative
    , wordlist =
        ./completion/wordlist
    }
, docker-compose =
    ./docker-compose/command
, docker =
    ./docker/command
, yarn =
    ./yarn/command
, direnv =
    ./direnv/command
, firefox =
    ./firefox/command
, pg_dump =
    ./pg_dump/command
, psql =
    ./psql/command
, run-mailcap =
    ./run-mailcap/command
, stack =
    ./stack/command
, tmux =
    ./tmux/command
, xdg-open =
    ./xdg-open/command
}
