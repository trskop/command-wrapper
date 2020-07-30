-- vim: filetype=dhall

let bash-completion-script-wrapper =
        ../completion/bash-completion-script-wrapper/package.dhall sha256:8f7fbe97c657d747d7cb0eb6de8d671cc37a9262196db66af236fc03f6ae2278
      ? ../completion/bash-completion-script-wrapper/package.dhall

let wrappedCompletionScript =
      https://raw.githubusercontent.com/trskop/neovim-remote/05fe6d0f4858e07bfe275e60bf8a1b29cb2b48d0/contrib/completion.bash sha256:db12d40572cebd14d9abbc75ca43a55815921b0e059733a40da8ee1876d9b15f as Text

let completion-script =
      bash-completion-script-wrapper.template
        bash-completion-script-wrapper.Options::{
        , completionFor = "nvr"
        , entryPoint = "_nvr_opts_completions"
        , completion = Some wrappedCompletionScript
        }

in  completion-script
