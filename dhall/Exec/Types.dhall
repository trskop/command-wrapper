{ docker =
    { GlobalOptions =
        ./docker/DockerGlobalOptions
    , ExecOptions =
        ./docker/DockerExecOptions
    , RunOptions =
        ./docker/DockerRunOptions
    }
, docker-compose =
    { GlobalOptions =
        ./docker-compose/GlobalOptions
    , DownOptions =
        ./docker-compose/DownOptions
    , UpOptions =
        ./docker-compose/UpOptions
    , Action =
        ./docker-compose/Action
    }
, nix =
    { Command = ./nix/Command/Type }
, ssh =
    { ConnectTo =
        ./ssh/ConnectTo
    , DynamicForwardingOptions =
        ./ssh/DynamicForwardingOptions
    , Forwarding =
        ./ssh/Forwarding
    , ForwardingOptions =
        ./ssh/ForwardingOptions
    , ListenOn =
        ./ssh/ListenOn
    , Options =
        ./ssh/Options
    }
}
