-- vim: filetype=dhall

{ toShell =
      ./to-shell sha256:1c04acd702d77136948fff8dc03af084322049a6cbcfdd6add145eebc48fbfbb
    ? ./to-shell
, verbosityOptions =
      ./verbosityOptions sha256:53abdd9ed8f27c0d175efc6b33e0a72d1d77661554e3e79e2a23d2c1252aa9a9
    ? ./verbosityOptions
, optionalOptions =
      ./optionalOptions sha256:f9626a1ef8209d45703cc7ac05aee40c9e96f5ccc2c406cdbd68eafd6d3234f0
    ? ./optionalOptions
, optionalFlags =
      ./optionalFlags sha256:dbb96b960dad5036585e8d0de1d15204a281d519ac9f59b33697100d62b5798c
    ? ./optionalFlags
, optionalEnvironmentVariables =
      ./optionalEnvironmentVariables sha256:97689a41f6b15c075cf0cfef3adc169b77f5afea52c5dadf8666a553b8ef2022
    ? ./optionalEnvironmentVariables
, colourOutputOptions =
      ./colourOutputOptions sha256:33260b6eaaf2a75fa0261a5e4b6dd3344406907f29fe2ff19838d55d1d18e80c
    ? ./colourOutputOptions
, colorOption =
      ./colorOption sha256:1974a5a3f18b441b9eae2f7cea20168ab26a97d450ad85a9e6160cea8538a170
    ? ./colorOption
}
