-- vim: filetype=dhall

{ ConnectTo =
      ./ConnectTo/package.dhall sha256:8334daae3b72ea29f477fbbb7cedca0908dd50f69cc873a527c276d2ca0217a8
    ? ./ConnectTo/package.dhall
, DynamicForwardingOptions =
      ./DynamicForwardingOptions/package.dhall sha256:ecbc598ac7e760fe9871dcc30427a8e01000319f1f75c2a81491d27ec237a81e
    ? ./DynamicForwardingOptions/package.dhall
, Forwarding =
      ./Forwarding/package.dhall sha256:81d6bdae4d9eb4b3acea55f8543cbbc26455b2b26ddeab5ab9fa21c99c00221d
    ? ./Forwarding/package.dhall
, ForwardingOptions =
      ./ForwardingOptions/package.dhall sha256:8b5eae02bdab20faa667a08818eda955c48d3e648323f1ce278c6a886f0f37a5
    ? ./ForwardingOptions/package.dhall
, ListenOn =
      ./ListenOn/package.dhall sha256:05ae45a0f648b05824c0071ab5aa88f0f399ada0174ce6562dfc6f05599a7a34
    ? ./ListenOn/package.dhall
, Options =
      ./Options/package.dhall sha256:c902a4c67c33d5309681e64ab87b1628a64c184f40dc227df2967ae978d67b23
    ? ./Options/package.dhall
, command =
      ./command sha256:d783d6a838c2552defa3abf757986141345a1ee761ff75449c754acc173d6cdc
    ? ./command
}
