-- vim: filetype=dhall
--
-- Generate `~/.bashrc` snippet that caches Command Wrapper completion script.
-- See top-level `README.md` file for more information.

  λ(toolsetNames : List Text)
→ let toolsets =
        List/fold
          Text
          toolsetNames
          Text
          (λ(name : Text) → λ(names : Text) → "${Text/show name} ${names}")
          ""
  
  in  ''
      # shellcheck shell=bash
      
      declare toolsetCacheDir='''
      declare toolsetCompletionFile='''
      for toolset in ${toolsets}; do
        toolsetCacheDir="''${XDG_CACHE_HOME:-''${HOME}/.cache}/''${toolset}"
        toolsetCompletionFile="''${toolsetCacheDir}/completion.bash"
        if [[ ! -e "''${toolsetCompletionFile}" ]]; then
            mkdir -p "''${toolsetCacheDir}"
        
            # This relies on the fact that Command Wrapper uses atomic write
            # operation to create the output file.
            "''${toolset}" completion --script --shell=bash \
                --output="''${toolsetCompletionFile}"
        fi
        
        # shellcheck source=/dev/null
        source "''${toolsetCompletionFile}"
      done
      unset -v toolset toolsetCacheDir toolsetCompletionFile
      ''
