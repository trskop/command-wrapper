-- vim: filetype=dhall

{ ConnectTo =
      ./ConnectTo sha256:e8382f5875b4a311bd30919586963a291cd2827be369dbad7cc6247212f2f96d
    ? ./ConnectTo
, DynamicForwardingOptions =
      ./DynamicForwardingOptions sha256:d4ebcc6427498a1408b5772b63cbace514cac027d0c6d9cddda6095529aa8a56
    ? ./DynamicForwardingOptions
, Forwarding =
      ./Forwarding sha256:434b5e6be35d6dd97cd29665c8c8855be653a25396174de07ec3c158dd8e4cdb
    ? ./Forwarding
, ForwardingOptions =
      ./ForwardingOptions sha256:ef6300632529b203c5a24b33d92ed0c5c4d05cd0c72655ce768271a2e6950f9f
    ? ./ForwardingOptions
, ListenOn =
      ./ListenOn sha256:8f9d7c5e14dcc61cb0437a17ec268328d44a9b1aa1c130c1a9b48e55644c8d0e
    ? ./ListenOn
, Options =
      ./Options sha256:47f3b65966d3c1b11183ae79b2d435931af9d26f334ebe04fa303233423f708f
    ? ./Options
, command =
      ./command sha256:c354bb96a55f747d56592f24f18975e7c9f4919ee4990cba35a837a17178bbe7
    ? ./command
, defaultOptions =
      ./defaultOptions sha256:0f1fd740c4c18336a6e024f9d2868b6079b28da5d7d07f98a6cb57285e0ee9fb
    ? ./defaultOptions
, options =
      ./options sha256:2b31c73c5377c9715f99fa310d4ea70c5a2c838692a221f5302aadd8c0c64724
    ? ./options
}
