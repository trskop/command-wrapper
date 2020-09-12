-- vim: filetype=dhall

let ProxySettings =
      { Type =
            ./Type.dhall sha256:6ab5e997b4b51ec84ec2c002c400f173d60e3863e088bf2c2b92028a39196825
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:6814645e2843e24ffa24c18c1abb7f63bded053fba95a7e2b2e0949f8f41e21d
          ? ./default.dhall
      }

let EnvironmentVariable =
        ../../CommandWrapper/EnvironmentVariable/package.dhall sha256:1250426124fe1a06fce13386a45e803655f7fe419fb372bd4a5157993350b199
      ? ../../CommandWrapper/EnvironmentVariable/package.dhall

let Environment =
        ../../CommandWrapper/Environment/package.dhall sha256:fd6671bdf4aec8a9bf066dd15cd0b97c68d2b6d6170676aed7ce1248f9533db1
      ? ../../CommandWrapper/Environment/package.dhall

let Prelude =
        ../../Prelude/package.dhall sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028
      ? ../../Prelude/package.dhall

let toEnvironmentVariable =
      λ(entry : Prelude.Map.Entry Text (Optional Text)) →
        merge
          { None = Environment.empty
          , Some = λ(value : Text) → [ { name = entry.mapKey, value } ]
          }
          entry.mapValue

let test0 =
        assert
      :   toEnvironmentVariable { mapKey = "FOO", mapValue = None Text }
        ≡ Environment.empty

let test1 =
        assert
      :   toEnvironmentVariable { mapKey = "FOO", mapValue = Some "bar" }
        ≡ [ { name = "FOO", value = "bar" } ]

let test2 =
        assert
      :   Prelude.List.concatMap
            (Prelude.Map.Entry Text (Optional Text))
            EnvironmentVariable.Type
            toEnvironmentVariable
            (toMap { FOO = Some "foo", BAR = None Text })
        ≡ [ { name = "FOO", value = "foo" } ]

let -- Usage of proxy environment variables is inconsistent. Some tools require
    -- all uppercase and some all lowercase. For that reason it is a good
    -- practice to export both variants.
    toEnvironment =
      λ(proxySettings : ProxySettings.Type) →
        let noProxy =
              if    Prelude.List.null Text proxySettings.noProxy
              then  None Text
              else  Some (Prelude.Text.concatSep "," proxySettings.noProxy)

        in  Prelude.List.concatMap
              (Prelude.Map.Entry Text (Optional Text))
              EnvironmentVariable.Type
              toEnvironmentVariable
              ( toMap
                  { http_proxy = proxySettings.httpProxy
                  , HTTP_PROXY = proxySettings.httpProxy
                  , https_proxy = proxySettings.httpsProxy
                  , HTTPS_PROXY = proxySettings.httpsProxy
                  , ftp_proxy = proxySettings.ftpProxy
                  , FTP_PROXY = proxySettings.ftpProxy
                  , rsync_proxy = proxySettings.rsyncProxy
                  , RSYNC_PROXY = proxySettings.rsyncProxy
                  , all_proxy = proxySettings.allProxy
                  , ALL_PROXY = proxySettings.allProxy
                  , no_proxy = noProxy
                  , NO_PROXY = noProxy
                  }
              )

let test3 = assert : toEnvironment ProxySettings::{=} ≡ Environment.empty

let test4 =
        assert
      :   toEnvironment
            ProxySettings::{
            , allProxy = Some "socks5://localhost:1080"
            , noProxy = [ "localhost", "127.0.0.0/8" ]
            }
        ≡ [ { name = "ALL_PROXY", value = "socks5://localhost:1080" }
          , { name = "NO_PROXY", value = "localhost,127.0.0.0/8" }
          , { name = "all_proxy", value = "socks5://localhost:1080" }
          , { name = "no_proxy", value = "localhost,127.0.0.0/8" }
          ]

in  toEnvironment : ProxySettings.Type → Environment.Type
