-- vim: filetype=dhall

let script =
      { url =
          let version =
                    ../version.dhall sha256:b6ada2201180818b57cbb98dd91b2489ba55fa7c3152b9b73c4fe8235f74ba39
                  ? ../version.dhall
                : Text

          in  "https://raw.githubusercontent.com/trskop/command-wrapper/${version}/command-wrapper/dhall/Exec/nix/completion-script.dhall"
      , hash =
              ./completion-script.hash.dhall sha256:a0b5a8befdc43dfdbd1da2a85b00ec1e0d7e2f8005576a25732b6f8dceddfdd4
            ? ./completion-script.hash.dhall
          : Text
      }

let Command =
      { Type =
            ./Command/Type.dhall sha256:dc2b111daadb4265396dc562b757f208189242a884e704a4298686ce8984af27
          ? ./Command/Type.dhall
      , show =
            ./Command/show.dhall sha256:802eda931b470db5c62b8c19260b9da1c87f5ea05ef5317ee241d87d9bc05961
          ? ./Command/show.dhall
      }

let wrapper =
        ../completion/wrapper.dhall sha256:2581cfe8c184898fe6b04aedad5ad9c1be34154b3a795ecab17ee19c899e5c8b
      ? ../completion/wrapper.dhall

in  λ(toolset : Text) →
    λ(command : Command.Type) →
      wrapper
        { toolset
        , expression = "${script.url} ${script.hash}"
        , command = Command.show command
        }
