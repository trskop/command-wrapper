-- vim: filetype=dhall
--
-- ```
-- psql [OPTIONS] [CONNECT_OPTIONS] [EXTRA_OPTIONS] [ARGUMENTS]
-- ```

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
      { Type =
            ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
          ? ../../CommandWrapper/Verbosity/Type
      , fold =
            ../../CommandWrapper/Verbosity/fold sha256:4dac2c264a2531d569ad0e5f712a1cd2d17b51ecdc502cc72f19937bf4733b1e
          ? ../../CommandWrapper/Verbosity/fold
      }

let ConnectToDatabase =
      { Type =
            ../../CommandWrapper/ConnectToDatabase/Type sha256:49946ad4b88108992adba47d0b212add49946772d8d8dca8b48ddb2b92635b46
          ? ../../CommandWrapper/ConnectToDatabase/Type
      , default =
            ../../CommandWrapper/ConnectToDatabase/default sha256:74ee5000319febdaf2165c91a11e326f3f17eddad687064809d696c5aaee2f32
          ? ../../CommandWrapper/ConnectToDatabase/default
      }

let Options =
        ./Options/package.dhall sha256:74793f048c90cdfa08280f490b20fa7a147dfbc724f45cd9df8aafe75f470320
      ? ./Options/package.dhall

let amendOptions =
      λ(options : Options.Type) →
      λ(verbosity : Verbosity.Type) →
      λ(colourOutput : ColourOutput) →
          options
        ⫽ { quiet =
              if    options.quiet
              then  options.quiet
              else  Verbosity.fold
                      Bool
                      { Silent = True
                      , Normal = False
                      , Verbose = False
                      , Annoying = False
                      }
                      verbosity
          , verbosity =
              let PgVerbosity = < default | verbose | terse >
              
              in  merge
                    { None =
                        Verbosity.fold
                          (Optional PgVerbosity)
                          { Silent = Some PgVerbosity.terse
                          , Normal = Some PgVerbosity.default
                          , Verbose = Some PgVerbosity.verbose
                          , Annoying = Some PgVerbosity.verbose
                          }
                          verbosity
                    , Some = λ(_ : < default | verbose | terse >) → Some _
                    }
                    options.verbosity
          , showContext =
              let ShowContext = < always | errors | never >
              
              in  merge
                    { None =
                        Verbosity.fold
                          (Optional ShowContext)
                          { Silent = None ShowContext
                          , Normal = None ShowContext
                          , Verbose = None ShowContext
                          , Annoying = Some ShowContext.always
                          }
                          verbosity
                    , Some = λ(_ : ShowContext) → Some _
                    }
                    options.showContext
          , colour =
              merge
                { None = Some colourOutput
                , Some = λ(_ : ColourOutput) → Some _
                }
                options.colour
          }

let connectToDatabaseOptions =
      λ(connect : ConnectToDatabase.Type) →
          [ "--host=${connect.hostname}"
          , "--username=${connect.username}"
          , "--dbname=${connect.database}"
          ]
        # merge
            { None = [] : List Text
            , Some = λ(port : Natural) → [ "--port=${Natural/show port}" ]
            }
            connect.port

let command =
      λ(unalteredOptions : Options.Type) →
      λ(connect : Optional ConnectToDatabase.Type) →
      λ(extraOptions : List Text) →
      λ(verbosity : Verbosity.Type) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        let options = amendOptions unalteredOptions verbosity colourOutput
        
        in  ExecCommand::{
            , command = "psql"
            , arguments =
                  Options.toArguments options
                # merge
                    { None = [] : List Text, Some = connectToDatabaseOptions }
                    connect
                # extraOptions
                # arguments
            , environment = Options.toEnvironment options
            , workingDirectory = options.workingDirectory
            }

let test0 =
        assert
      :   command
            Options::{=}
            (None ConnectToDatabase.Type)
            ([] : List Text)
            Verbosity.Type.Normal
            ColourOutput.Auto
            ([] : List Text)
        ≡ ExecCommand::{
          , command = "psql"
          , arguments = [ "--set=VERBOSITY=default" ]
          , environment = [ { name = "PG_COLOR", value = "auto" } ]
          }

let test1 =
        assert
      :   command
            Options::{=}
            ( Some
                ConnectToDatabase::{
                , hostname = "localhost"
                , database = "events"
                , username = "admin"
                }
            )
            ([] : List Text)
            Verbosity.Type.Normal
            ColourOutput.Auto
            ([] : List Text)
        ≡ ExecCommand::{
          , command = "psql"
          , arguments =
            [ "--set=VERBOSITY=default"
            , "--host=localhost"
            , "--username=admin"
            , "--dbname=events"
            ]
          , environment = [ { name = "PG_COLOR", value = "auto" } ]
          }

in    command
    : Options.Type →
      Optional ConnectToDatabase.Type →
      ∀(extraOptions : List Text) →
      Verbosity.Type →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
