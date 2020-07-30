-- vim: filetype=dhall

let ForwardingOptions =
        ../ForwardingOptions/Type.dhall sha256:ef6300632529b203c5a24b33d92ed0c5c4d05cd0c72655ce768271a2e6950f9f
      ? ../ForwardingOptions/Type.dhall

let DynamicForwardingOptions =
        ../DynamicForwardingOptions/Type.dhall sha256:d4ebcc6427498a1408b5772b63cbace514cac027d0c6d9cddda6095529aa8a56
      ? ../DynamicForwardingOptions/Type.dhall

in  < Local : ForwardingOptions
    | Remote : ForwardingOptions
    | Dynamic : DynamicForwardingOptions
    >
