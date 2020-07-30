-- vim: filetype=dhall
--
-- JIRA command line tool: https://github.com/go-jira/jira
--
-- How to get API token is documented on
-- <https://confluence.atlassian.com/cloud/api-tokens-938839638.html>.
-- TL;DR: API token can be created on
-- <https://id.atlassian.com/manage/api-tokens>.

let ExecCommand =
      { Type =
            ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
          ? ../../CommandWrapper/ExecCommand/Type
      , default =
            ../../CommandWrapper/ExecCommand/default sha256:c3a088ca2b090c91d5d630c4e01f4b4fbd0136f5bb1251f6828698e5180685b2
          ? ../../CommandWrapper/ExecCommand/default
      }

let Verbosity =
        ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../CommandWrapper/Verbosity/Type

let ColourOutput =
        ../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../CommandWrapper/ColourOutput/Type

let Options =
      { Type =
            ./Options/Type.dhall sha256:89bafb720da9507168156fc176d0216d081875e79b1a23f8c301fdeb3721bb6d
          ? ./Options/Type.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:70cb6da8907898be8fca141df6695546fbb878cade8bd2559b144868f3a3338f
          ? ./Options/toArguments.dhall
      , toEnvironment =
            ./Options/toEnvironment.dhall sha256:2ee7820542005468ab46f11a81b1a0cdbdd5731e7e26c4708f055bd19dd1bb14
          ? ./Options/toEnvironment.dhall
      }

let command =
      λ(options : Options.Type) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        ExecCommand::{
        , command = "jira"
        , arguments = Options.toArguments options (Some verbosity) # arguments
        , environment = Options.toEnvironment options
        }

in    command
    : Options.Type →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
