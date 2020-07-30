-- vim: filetype=dhall

{ ConnectTo =
      ./ConnectTo/package.dhall sha256:0b6dd929e92a621c748ac5c069abe33ba4affe257d9fdce282af0aed54916302
    ? ./ConnectTo/package.dhall
, DynamicForwardingOptions =
      ./DynamicForwardingOptions/package.dhall sha256:ecbc598ac7e760fe9871dcc30427a8e01000319f1f75c2a81491d27ec237a81e
    ? ./DynamicForwardingOptions/package.dhall
, Forwarding =
      ./Forwarding/package.dhall sha256:81d6bdae4d9eb4b3acea55f8543cbbc26455b2b26ddeab5ab9fa21c99c00221d
    ? ./Forwarding/package.dhall
, ForwardingOptions =
      ./ForwardingOptions/package.dhall sha256:59b55245902362c48c6016803315e26ed560f598f405de4b153f9e060c0045fd
    ? ./ForwardingOptions/package.dhall
, ListenOn =
      ./ListenOn/package.dhall sha256:05ae45a0f648b05824c0071ab5aa88f0f399ada0174ce6562dfc6f05599a7a34
    ? ./ListenOn/package.dhall
, Options =
      ./Options/package.dhall sha256:c9ec017070918c472f09b7ca65d505de2855d90fb600aafb2988b4f5f2738d73
    ? ./Options/package.dhall
, SshTo =
      ./SshTo/package.dhall sha256:67545f844076e4c3491d6d76bce30e54fa5b8e8e5fcf35788145e61a93a7b2be
    ? ./SshTo/package.dhall
, command =
      ./command.dhall sha256:e5b3710e0a6f0208df73c9c2405b01bc8839ca65e8f766c6258aa417d6a9589b
    ? ./command.dhall
}
