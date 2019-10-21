-- vim: filetype=dhall
--
-- Defaults and library of useful tools for toolset and external subcommands
-- bundled with Command Wrapper.

{ CdConfig =
      ./CdConfig/package.dhall sha256:2b1f0e05455adf874da059af0ec469968501de7d2f333d29346e14d122ee41b9
    ? ./CdConfig/package.dhall
, ColourOutput =
      ./ColourOutput/package.dhall sha256:ab063b4e29e5237c1bbb2c4ed07ed3cc80096c9cd2f9978d727d81ac1f1c8c68
    ? ./ColourOutput/package.dhall
, Command =
      ./Command/package.dhall sha256:3de0160a4821c2d746a2a0c859857c7dce16fdcb75e1aa18a3e5ff48b310c462
    ? ./Command/package.dhall
, CommandWithEnvironment =
      ./CommandWithEnvironment/package.dhall sha256:fb3dbcfcc02c02cf75b535b6633f1cfb032502aed71e148693c950d902772e01
    ? ./CommandWithEnvironment/package.dhall
, ConnectToDatabase =
      ./ConnectToDatabase/package.dhall sha256:75de702e5d74d70d6f5e08473d25d5ba2c3040febdc86b2226f55ec0423ebf71
    ? ./ConnectToDatabase/package.dhall
, Editor =
      ./Editor/package.dhall sha256:ea947ca2fa9dec681385344190f8373da2d4ced8556cce9096c354f9265992f5
    ? ./Editor/package.dhall
, EnvironmentVariable =
      ./EnvironmentVariable/package.dhall sha256:a2ed60e366c315c84a20787193d11752f8678dfcd597fb740eb04ce609ed1088
    ? ./EnvironmentVariable/package.dhall
, ExecCommand =
      ./ExecCommand/package.dhall sha256:dc4bc6ac7b09f9d64ab848b55133c48908e848192d236446061ac284d88b62b1
    ? ./ExecCommand/package.dhall
, ExecConfig =
      ./ExecConfig/package.dhall sha256:54303713892c8eed71abdcc4437728476ae67e78f73ff539cd52115bc083b28b
    ? ./ExecConfig/package.dhall
, ExecNamedCommand =
      ./ExecNamedCommand/package.dhall sha256:18552a3a1578677bc8ac0d6848ae783fb99daa2e338179ae1b8e5ac6e24ffaff
    ? ./ExecNamedCommand/package.dhall
, ExitCode =
      ./ExitCode/package.dhall sha256:53545bbc3d4e2e6a6c2272259c3d34d8a91fdbc5b4cdda626a886adbde9e902e
    ? ./ExitCode/package.dhall
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
      ./Shell/package.dhall sha256:a6ef0d2ad631ea9c9a2edc8d93290dc02b5d7bad7c51b79cb3c6db5baa439511
    ? ./Shell/package.dhall
, SubcommandAlias =
      ./SubcommandAlias/package.dhall sha256:edaaf5b2bcdb52389af07101a7cda57000e2acba33b49cac94ea11b5e572b1ab
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
, config =
    { skel =
        { defaults =
              ./skel/defaults sha256:f7bbe6987589d64f01d727392db83726e64c67e263eb45adf2b5655ad1a442f6
            ? ./skel/defaults
        , default-bash-skel =
              ./skel/default-bash-skel sha256:00b5d1f752d64aed9f2347df3f7b38c296ec237744b1999fa301efb68ebfe478
            ? ./skel/default-bash-skel
        , default-dhall-skel =
              ./skel/default-dhall-skel sha256:340030d5c2186d2292f10e6d3dc26e4027aee6b2d8eb70a5915f2854dfd94180
            ? ./skel/default-dhall-skel
        , default-haskell-skel =
              ./skel/default-haskell-skel sha256:99efef7061c9b8c0fc5c3e73ba79be4fb9f23ce82e3accb4b913a3e4c6edf5dd
            ? ./skel/default-haskell-skel
        }
    }
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
