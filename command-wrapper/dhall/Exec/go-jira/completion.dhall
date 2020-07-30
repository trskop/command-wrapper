-- vim: filetype=dhall

let script =
      { url =
          let version =
                    ../version.dhall sha256:b6ada2201180818b57cbb98dd91b2489ba55fa7c3152b9b73c4fe8235f74ba39
                  ? ../version.dhall
                : Text

          in  "https://raw.githubusercontent.com/trskop/command-wrapper/${version}/command-wrapper/dhall/Exec/go-jira/completion-script.dhall"
      , hash =
              ./completion-script.hash.dhall sha256:5bbc772887c473113e2a8f4bb62d0febf8d49af52131c3965041a0e68bcb2981
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
        , command = "jira"
        }
