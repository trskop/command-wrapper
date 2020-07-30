-- vim: filetype=dhall

let script =
      { url =
          let version =
                    ../version.dhall sha256:b6ada2201180818b57cbb98dd91b2489ba55fa7c3152b9b73c4fe8235f74ba39
                  ? ../version.dhall
                : Text

          in  "https://raw.githubusercontent.com/trskop/command-wrapper/${version}/command-wrapper/dhall/Exec/youtube-dl/completion-script.dhall"
      , hash =
              ./completion-script.hash.dhall sha256:624fa8ea5aa670a58d76993b324e51214a5bc8981ddc493e5ce4fe9ebdd84af6
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
        , command = "youtube-dl"
        }
