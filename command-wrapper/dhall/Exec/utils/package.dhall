-- vim: filetype=dhall

{ toShell =
      ./toShell sha256:ae09eb21c74be302fb49c4cafa7f3720f65d0d41a4156ecb1f7d3f08b98f82f9
    ? ./toShell
, verbosityOptions =
      ./verbosityOptions sha256:53abdd9ed8f27c0d175efc6b33e0a72d1d77661554e3e79e2a23d2c1252aa9a9
    ? ./verbosityOptions
, optionalOptions =
      ./optionalOptions sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
    ? ./optionalOptions
, optionalFlags =
      ./optionalFlags sha256:0dba774441dd92889f9a2a9819a6bca5ad7d1d891fbac9fa5c284367ca9fec33
    ? ./optionalFlags
, optionalEnvironmentVariables =
      ./optionalEnvironmentVariables sha256:2565c61b55b84de6e75b663f3561c806bcebc193ff662c644150dc46ebe05c0b
    ? ./optionalEnvironmentVariables
, colourOutputOptions =
      ./colourOutputOptions sha256:33260b6eaaf2a75fa0261a5e4b6dd3344406907f29fe2ff19838d55d1d18e80c
    ? ./colourOutputOptions
, colorOption =
      ./colorOption sha256:1974a5a3f18b441b9eae2f7cea20168ab26a97d450ad85a9e6160cea8538a170
    ? ./colorOption
}
