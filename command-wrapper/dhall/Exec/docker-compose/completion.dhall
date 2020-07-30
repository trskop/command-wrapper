-- vim: filetype=dhall

let script =
      { url =
          let version =
                    ../version.dhall sha256:b6ada2201180818b57cbb98dd91b2489ba55fa7c3152b9b73c4fe8235f74ba39
                  ? ../version.dhall
                : Text

          in  "https://raw.githubusercontent.com/trskop/command-wrapper/${version}/command-wrapper/dhall/Exec/docker-compose/completion-script.dhall"
      , hash =
              ./completion-script.hash.dhall sha256:5c3194e5cde6f556a6abcfa999eca4b2db9a58544a26fbec300ca9a2877af2d9
            ? ./completion-script.hash.dhall
          : Text
      }

let wrapper =
        ../completion/wrapper.dhall sha256:2581cfe8c184898fe6b04aedad5ad9c1be34154b3a795ecab17ee19c899e5c8b
      ? ../completion/wrapper.dhall

in  λ(toolset : Text) →
      wrapper
        { toolset
        , expression = "${script.url} ${script.hash}"
        , command = "docker-compose"
        }
