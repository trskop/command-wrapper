-- vim: filetype=dhall

{ ConnectTo =
      ./ConnectTo/package.dhall sha256:8334daae3b72ea29f477fbbb7cedca0908dd50f69cc873a527c276d2ca0217a8
    ? ./ConnectTo/package.dhall
, DynamicForwardingOptions =
      ./DynamicForwardingOptions/package.dhall sha256:b43c1f1de7a31374bd865d41f643dd7e6b121a900f39c7658e7aca08433736ce
    ? ./DynamicForwardingOptions/package.dhall
, Forwarding =
      ./Forwarding/package.dhall sha256:e884ddb826819a204c57a8efb922e904bb88ed0b6196187a6e0d7622c7f64e6f
    ? ./Forwarding/package.dhall
, ForwardingOptions =
      ./ForwardingOptions/package.dhall sha256:2bca691b7978cc4223aa9ceaaa68bbeb9094b86f6f8cb8e01f2360b2521c92e8
    ? ./ForwardingOptions/package.dhall
, ListenOn =
      ./ListenOn/package.dhall sha256:05ae45a0f648b05824c0071ab5aa88f0f399ada0174ce6562dfc6f05599a7a34
    ? ./ListenOn/package.dhall
, Options =
      ./Options/package.dhall sha256:e1a4d5858379a712dc9a20c6eaae1c02465ad1bb40c15aa1d74629e99aecdcaa
    ? ./Options/package.dhall
, command =
      ./command sha256:5683e3567a3445a18c86100dc5f29cffdb1a212e1c1c04dbada97b94b92d75a0
    ? ./command
}
