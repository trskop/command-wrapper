-- vim: filetype=dhall
--
-- Types, defaults and utilities for configuring Command Wrapper toolset and
-- external subcommands installed with it by default.

{ CdConfig =
      ./CdConfig/package.dhall sha256:ef3299a1b9aa40cef88f51364deed4d82fdc52aeb9505eab3fd61e85febb6ba2
    ? ./CdConfig/package.dhall
, ColourOutput =
      ./ColourOutput/package.dhall sha256:8fd5555f273cd4424f8fe2003154c4e36ea657bb5a2642d3677c4756b14c32e8
    ? ./ColourOutput/package.dhall
, Command =
      ./Command/package.dhall sha256:9fe4745e50e55ee1670a26ae2476c655c3d52ea92e920911b093a567de2099fb
    ? ./Command/package.dhall
, CommandWithEnvironment =
      ./CommandWithEnvironment/package.dhall sha256:274b4f450abe8835cace52f07cd6d50fc0b649ff2ffbcbf4c2258332c1ee5697
    ? ./CommandWithEnvironment/package.dhall
, ConnectToDatabase =
      ./ConnectToDatabase/package.dhall sha256:a392e73ac310bdab2775458e638b656cf1a0acd38e1371ff8d6deb03acb52a94
    ? ./ConnectToDatabase/package.dhall
, Editor =
      ./Editor/package.dhall sha256:bffb77e129724406d2b21de0e756f2720b936ec9d2756568382fab12f9116722
    ? ./Editor/package.dhall
, Environment =
      ./Environment/package.dhall sha256:fd6671bdf4aec8a9bf066dd15cd0b97c68d2b6d6170676aed7ce1248f9533db1
    ? ./Environment/package.dhall
, EnvironmentVariable =
      ./EnvironmentVariable/package.dhall sha256:1250426124fe1a06fce13386a45e803655f7fe419fb372bd4a5157993350b199
    ? ./EnvironmentVariable/package.dhall
, ExecCommand =
      ./ExecCommand/package.dhall sha256:3d1f1db23e794116bfc80efad94a291087792b0a96d8f5be5b1c9276657d0663
    ? ./ExecCommand/package.dhall
, ExecConfig =
      ./ExecConfig/package.dhall sha256:54303713892c8eed71abdcc4437728476ae67e78f73ff539cd52115bc083b28b
    ? ./ExecConfig/package.dhall
, ExecNamedCommand =
      ./ExecNamedCommand/package.dhall sha256:e6f087dc4b21931cb331f553ee0eb4bc9ab88c42fb043fcbbf40809620c243b0
    ? ./ExecNamedCommand/package.dhall
, ExitCode =
      ./ExitCode/package.dhall sha256:53545bbc3d4e2e6a6c2272259c3d34d8a91fdbc5b4cdda626a886adbde9e902e
    ? ./ExitCode/package.dhall
, FileTemplate =
      ./FileTemplate/package.dhall sha256:f1d9b72bbd054c205b654a7c8ff955d1b654c87cbc58fc37307f7363d68bf755
    ? ./FileTemplate/package.dhall
, List =
      ./List/package.dhall sha256:bc5db5456e29be5d2481d95f3eabb056db179ecfab16b1ec8d0d73ae4e10f29b
    ? ./List/package.dhall
, NonEmpty =
      ./NonEmpty/package.dhall sha256:d45ec932432060036fd69ba8023cbaf48e63c8fca6e9387023a1d3e869d6ec67
    ? ./NonEmpty/package.dhall
, NotifyWhen =
      ./NotifyWhen/package.dhall sha256:9b0aa668c037d6c7f33eff10551dd07275d6f6d802b8fd686ab630ae36b4fec1
    ? ./NotifyWhen/package.dhall
, Optional =
      ./Optional/package.dhall sha256:74bf40429cc9fc088e6200cd931e0d4021b729da482ee37ccfeeb962b1b7874e
    ? ./Optional/package.dhall
, Schema =
      ./Schema/package.dhall sha256:f3e148dc04e97452d1e5fb8ff0b819c8bc0193a29733f4c1939901fe8ffe1c3a
    ? ./Schema/package.dhall
, Shell =
      ./Shell/package.dhall sha256:ec32d80f4962cf4f2a5be99dfbc2534c15431a40bad5142448fb6ec2bd8ef0a4
    ? ./Shell/package.dhall
, SkelConfig =
      ./SkelConfig/package.dhall sha256:3bd0e0c4c2e36b2ab8097ddbba500311f702297b718729cdd0dbf05ceac37659
    ? ./SkelConfig/package.dhall
, SubcommandAlias =
      ./SubcommandAlias/package.dhall sha256:3c0eb761e752cf7f43a794586af41951faf744d5d6c8b7c00e0fb31f4e6a9ff2
    ? ./SubcommandAlias/package.dhall
, TerminalEmulator =
      ./TerminalEmulator/package.dhall sha256:f0c6d05ee9dd15770fc61522b1c8708472c90487eb9c361a9c6ff4ae72b1b6ce
    ? ./TerminalEmulator/package.dhall
, ToolsetConfig =
      ./ToolsetConfig/package.dhall sha256:09fcbd58608b2bba9303e694bbb3f727dde5292c27da0c6fb4f2c2580a437192
    ? ./ToolsetConfig/package.dhall
, Verbosity =
      ./Verbosity/package.dhall sha256:488f95a5a27b82653c5a759b592b08e16940d1698dcf956fcbd9c153cb2547f2
    ? ./Verbosity/package.dhall
, help =
  { command =
        ./help/command sha256:a4e9d3b8603dfe58f03c38651c10b094ac98c960c1a92dd0687159bbde7e268b
      ? ./help/command
  , metavar =
        ./help/metavar sha256:eeaf3da99e1d7890594c740170f85efcfb6d9f4437cbb5bfca1288bff47f292f
      ? ./help/metavar
  , option =
        ./help/option sha256:12cd9405555ed6b02370c5861d125983f73c81c575e306859b39bf35fb9bba7b
      ? ./help/option
  , plain =
        ./help/plain sha256:f6f89359a06818d8c2840715c32f9f88f8336e999a2871f3aa93337d03d30f10
      ? ./help/plain
  , value =
        ./help/value sha256:5f1f3fcc9e13c5576bd0b75e8619aa8b02269a01c9d148c6a520602adba87527
      ? ./help/value
  }
, utils.url =
  { defaultPort =
        ./url/defaultPort sha256:bfdc295856e05e3610d202d9c12f56f7c423de81cd6069f590786ca176b94df3
      ? ./url/defaultPort
  , emptyPath =
        ./url/emptyPath sha256:f08ed5225480d827ff3ce74b756afa6330f66d974d6f0d6160d767b5c45642aa
      ? ./url/emptyPath
  , mk =
        ./url/mk sha256:8549a8775cb00f9f860b0c176ff898cd024ad5e27b0be2515c33632c5f965f35
      ? ./url/mk
  , port =
        ./url/port sha256:d7e20173b3139c6c37b7b71a94a6ada30efe37db9e60ff9f25aa8292cc1d4502
      ? ./url/port
  , portToText =
        ./url/portToText sha256:108998e7c66481b0a2b0aa5d3a8514789575118ae7988b6113dc849dca4347df
      ? ./url/portToText
  }
}
