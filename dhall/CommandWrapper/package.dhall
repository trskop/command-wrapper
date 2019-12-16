-- vim: filetype=dhall
--
-- Types, defaults and utilities for configuring Command Wrapper toolset and
-- external subcommands installed with it by default.

{ CdConfig =
      ./CdConfig/package.dhall sha256:46884025399330c9b7843eee13d741bbfcb6bd31810cce94f8fa55262412f337
    ? ./CdConfig/package.dhall
, ColourOutput =
      ./ColourOutput/package.dhall sha256:8fd5555f273cd4424f8fe2003154c4e36ea657bb5a2642d3677c4756b14c32e8
    ? ./ColourOutput/package.dhall
, Command =
      ./Command/package.dhall sha256:9fe4745e50e55ee1670a26ae2476c655c3d52ea92e920911b093a567de2099fb
    ? ./Command/package.dhall
, CommandWithEnvironment =
      ./CommandWithEnvironment/package.dhall sha256:97ad640e7ac252d569805cbe324aa42d5ff46c7b8d323b216fbf8d9715ce12d6
    ? ./CommandWithEnvironment/package.dhall
, ConnectToDatabase =
      ./ConnectToDatabase/package.dhall sha256:a392e73ac310bdab2775458e638b656cf1a0acd38e1371ff8d6deb03acb52a94
    ? ./ConnectToDatabase/package.dhall
, Editor =
      ./Editor/package.dhall sha256:ea947ca2fa9dec681385344190f8373da2d4ced8556cce9096c354f9265992f5
    ? ./Editor/package.dhall
, Environment =
      ./Environment/package.dhall sha256:a948211dc39cc648f650c7dcf67e4352ffae2ec5f4f0254bef596d88e87cd946
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
      ./List/package.dhall sha256:1b97cbd979abcff98f78223e6320a6ba81c378392c9bc1476208832834c8aa68
    ? ./List/package.dhall
, NotifyWhen =
      ./NotifyWhen/package.dhall sha256:9b0aa668c037d6c7f33eff10551dd07275d6f6d802b8fd686ab630ae36b4fec1
    ? ./NotifyWhen/package.dhall
, Optional =
      ./Optional/package.dhall sha256:33857949625599d84fb64d5b77946655d38fa1e3120ed397f2166a106b09e5f7
    ? ./Optional/package.dhall
, Schema =
      ./Schema/package.dhall sha256:f3e148dc04e97452d1e5fb8ff0b819c8bc0193a29733f4c1939901fe8ffe1c3a
    ? ./Schema/package.dhall
, Shell =
      ./Shell/package.dhall sha256:ec32d80f4962cf4f2a5be99dfbc2534c15431a40bad5142448fb6ec2bd8ef0a4
    ? ./Shell/package.dhall
, SkelConfig =
      ./SkelConfig/package.dhall sha256:472af7056ef8b087d208ad17f7058afd341bdee98b8b1146de2eb48d6ef5612f
    ? ./SkelConfig/package.dhall
, SubcommandAlias =
      ./SubcommandAlias/package.dhall sha256:3c0eb761e752cf7f43a794586af41951faf744d5d6c8b7c00e0fb31f4e6a9ff2
    ? ./SubcommandAlias/package.dhall
, TerminalEmulator =
      ./TerminalEmulator/package.dhall sha256:7af1af071f36f10b12d8f076016fad8f9d070e5da793f20e42a0f4ec074bd519
    ? ./TerminalEmulator/package.dhall
, ToolsetConfig =
      ./ToolsetConfig/package.dhall sha256:1c7797fd77cf4d0a802b04f42fc8d35188eb76bb26c55a88c2fb3228a168f416
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
, utils =
    { url =
        { defaultPort =
              ./url/defaultPort sha256:bfdc295856e05e3610d202d9c12f56f7c423de81cd6069f590786ca176b94df3
            ? ./url/defaultPort
        , emptyPath =
              ./url/emptyPath sha256:f08ed5225480d827ff3ce74b756afa6330f66d974d6f0d6160d767b5c45642aa
            ? ./url/emptyPath
        , mk =
              ./url/mk sha256:e9438a6467c75130cd3eb6dbfdfabc13a35b78f7cf5009de2564dd6799d429d0
            ? ./url/mk
        , port =
              ./url/port sha256:d7e20173b3139c6c37b7b71a94a6ada30efe37db9e60ff9f25aa8292cc1d4502
            ? ./url/port
        , portToText =
              ./url/portToText sha256:a1d2133ee6b8cd5a5f2bdbc439a80f4c0ceff48194617cd28c98a1a30ad917e3
            ? ./url/portToText
        }
    }
}
