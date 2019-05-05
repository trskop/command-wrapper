  λ(name : Text)
→ λ(toolset : Text)
→ λ(command : Text)
→ λ(subcommand : Optional Text)
→ { bash =
      let subcommandOption = Optional/fold Text subcommand Text
            ( λ(cmd : Text)
            → ''
              \
                          --subcommand="${cmd}"''
            )
            ""

      in  ''
          function _${name}()
          {
              COMP_WORDBREAKS="''${COMP_WORDBREAKS//=}"
              mapfile -t COMPREPLY < <(
                  COMMAND_WRAPPER_INVOKE_AS='${toolset}' "${command}" completion \
                      --index="''${COMP_CWORD}" ${subcommandOption} \
                      --shell=bash \
                      -- "''${COMP_WORDS[@]}"
              )
              [[ "''${COMPREPLY[0]}" == "''${COMPREPLY[0]%=}=" ]] && compopt -o nospace
          }
          complete -o filenames -F _${name} ${name}
          ''

  -- TODO: This code is just a best guess, it was never tested.
  , fish =
      let subcommandOption = Optional/fold Text subcommand Text
            (λ(cmd : Text) → "--subcommand='${cmd}'") ""

      in  ''
function _${name}
    set -l cl (commandline --tokenize --current-process)
    # Hack around fish issue #3934
    set -l cn (commandline --tokenize --cut-at-cursor --current-process)
    set -l cn (count $cn)
    set -l tmpline --index $cn ${subcommandOption} --shell=fish -- $cl
    for opt in (COMMAND_WRAPPER_INVOKE_AS='${toolset}' "${command}" completion $tmpline)
      if test -d $opt
        echo -E "$opt/"
      else
        echo -E "$opt"
      end
    end
end

complete --no-files --command "${name}" --arguments '(_${name})'
''

  -- TODO: This code is just a best guess, it was never tested.
  , zsh =
      let subcommandOption = Optional/fold Text subcommand Text
            ( λ(cmd : Text)
            → ''
              \
                          --subcommand="${cmd}"''
            )
            ""

      in  ''
#compdef ${name}

local completions
local word
local index=$((CURRENT - 1))

IFS=$'\n' completions=($(
    COMMAND_WRAPPER_INVOKE_AS='${toolset}' "${command}" completion \
        --index="''${index}" ${subcommandOption} \
        --shell=zsh \
        -- "''${words[@]}"
))

for word in $completions; do
  local -a parts

  # Split the line at a tab if there is one.
  IFS=$'\t' parts=($( echo $word ))

  if [[ -n $parts[2] ]]; then
     if [[ $word[1] == "-" ]]; then
       local desc=("$parts[1] ($parts[2])")
       compadd -d desc -- $parts[1]
     else
       local desc=($(print -f  "%-019s -- %s" $parts[1] $parts[2]))
       compadd -l -d desc -- $parts[1]
     fi
  else
    compadd -f -- $word
  fi
done
''
  }.{bash}  -- Temporary until other shells are supported.
