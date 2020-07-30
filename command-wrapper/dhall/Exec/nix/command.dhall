-- vim: filetype=dhall

let ExecCommand =
      { Type =
            ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
          ? ../../CommandWrapper/ExecCommand/Type
      , default =
            ../../CommandWrapper/ExecCommand/default sha256:c3a088ca2b090c91d5d630c4e01f4b4fbd0136f5bb1251f6828698e5180685b2
          ? ../../CommandWrapper/ExecCommand/default
      }

let ColourOutput =
        ../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../CommandWrapper/ColourOutput/Type

let Verbosity =
        ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../CommandWrapper/Verbosity/Type

let Options =
      { Type =
            ./Options/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
          ? ./Options/Type.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:73b3be19a87c872ba5c03e6daaf400233ee0c6e9f13ad9ea9e5b6c958c112bf8
          ? ./Options/toArguments.dhall
      , toEnvironment =
            ./Options/toEnvironment.dhall sha256:6fa0057412ff539cd27b042cfd1a59fcee06eb64fbff36ca904f0a3436619d35
          ? ./Options/toEnvironment.dhall
      }

let Command =
      { Type =
            ./Command/Type.dhall sha256:dc2b111daadb4265396dc562b757f208189242a884e704a4298686ce8984af27
          ? ./Command/Type.dhall
      , show =
            ./Command/show.dhall sha256:802eda931b470db5c62b8c19260b9da1c87f5ea05ef5317ee241d87d9bc05961
          ? ./Command/show.dhall
      }

in  λ(options : Options.Type) →
    λ(command : Command.Type) →
    λ(commandOptions : List Text) →
    λ(verbosity : Verbosity) →
    λ(colourOutput : ColourOutput) →
    λ(arguments : List Text) →
      ExecCommand::{
      , command = Command.show command
      , arguments = Options.toArguments options # commandOptions # arguments
      , workingDirectory = options.workingDirectory
      , environment = Options.toEnvironment options
      }
