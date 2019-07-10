{ docker-compose =
    { GlobalOptions =
        ./docker-compose/GlobalOptions
    , DownOptions =
        ./docker-compose/DownOptions
    , UpOptions =
        ./docker-compose/UpOptions
    , Action =
        ./docker-compose/Action
    }
, docker =
    { GlobalOptions =
        ./docker/DockerGlobalOptions
    , ExecOptions =
        ./docker/DockerExecOptions
    , RunOptions =
        ./docker/DockerRunOptions
    }
}
