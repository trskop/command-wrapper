-- vim: filetype=dhall

let script =
      { url =
          let version =
                    ../version.dhall sha256:b6ada2201180818b57cbb98dd91b2489ba55fa7c3152b9b73c4fe8235f74ba39
                  ? ../version.dhall
                : Text

          in  "https://raw.githubusercontent.com/trskop/command-wrapper/${version}/command-wrapper/dhall/Exec/nvr/completion-script.dhall"
      , hash =
              ./completion-script.hash.dhall sha256:97bc1961335852e836b2d23fb623762308238c944d1f0b0f707869a08e98b14a
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
        , command = "nvr"
        }
