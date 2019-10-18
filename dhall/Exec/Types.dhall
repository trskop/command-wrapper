-- vim: filetype=dhall

{ docker =
    { GlobalOptions = ./docker/DockerGlobalOptions
    , ExecOptions = ./docker/DockerExecOptions
    , RunOptions = ./docker/DockerRunOptions
    }
, docker-compose =
    { GlobalOptions = ./docker-compose/GlobalOptions
    , DownOptions = ./docker-compose/DownOptions
    , UpOptions = ./docker-compose/UpOptions
    , Action = ./docker-compose/Action
    }
, firefox =
    { Options = ./firefox/Options/Type
    , Profile = ./firefox/Profile/Type
    , Remote = ./firefox/Remote/Type
    , Open = ./firefox/Open/Type
    }
, go-jira = { Options = ./go-jira/Options }
, nix = { Command = ./nix/Command/Type }
, ssh =
    { ConnectTo = ./ssh/ConnectTo
    , DynamicForwardingOptions = ./ssh/DynamicForwardingOptions
    , Forwarding = ./ssh/Forwarding
    , ForwardingOptions = ./ssh/ForwardingOptions
    , ListenOn = ./ssh/ListenOn
    , Options = ./ssh/Options
    }
}
