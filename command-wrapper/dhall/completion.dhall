-- vim: filetype=dhall
--
-- Dhall templates for shell scripts that enable CommandWrapper comamnd line
-- completon for a specific toolset.
--
-- This whole script represents a function of the following type:
--
--    λ(shell : < Bash | Fish | Zsh >)
--  -- ^ Shell for which completion is generated.
--  → λ(toolsetName : Text)
--  -- ^ Name of the toolset for which completion is generated.
--  → λ(executable : Text)
--  -- ^ Absolute path to Command Wrapper executable.
--  → λ(subcommand : Optional Text)
--  -- ^ If present, then we are not generating completion for the toolset
--  -- itself, but for its subcommand. This is useful if we have an alias for
--  --
--  -- ```
--  -- alias foo='the_toolset foo'
--  -- ```
--  --
--  -- And we want to generate completion for `foo`.
--  → λ(aliases : List Text)
--  -- ^ If `subcommand` is not present, then this is the list of alternative
--  -- names (aliases) under which the toolset is known. For example, if we
--  -- have following alias:
--  --
--  -- ```
--  -- alias ts='the_toolset'
--  -- ```
--  --
--  -- Then `toolsetName` would be `the_toolset` and value of `aliases` would
--  -- be `[ "ts" ]`.
--  --
--  -- In case of `subcommand` being present then these are aliase under which
--  -- the toolset subcommand itself is known.  In case of:
--  --
--  -- ```
--  -- alias foo='the_toolset foo'
--  -- ```
--  --
--  -- Value of `aliases` would be `[ "foo" ]`.
--  → Text
--  -- ^ Generated shell script.
--
-- When Command Wrapper is invoked by command line completion we pass
-- `--verbosity=silent` so that error messages do not interfere with
-- completion.

let bashTemplate =
        λ(name : Text)
      → λ(toolset : Text)
      → λ(command : Text)
      → λ(subcommand : Optional Text)
      → λ(names : List Text)
      → let subcommandOption =
              Optional/fold
                Text
                subcommand
                Text
                (   λ(cmd : Text)
                  → ''
                    \
                    ${"            "}--subcommand="${cmd}" ''
                )
                ""

        let complete =
              λ(cmd : Text) → "complete -o filenames -F '_${name}' '${cmd}'"

        let completeNames =
              List/fold
                Text
                names
                Text
                (λ(alias : Text) → λ(t : Text) → complete alias ++ "\n" ++ t)
                ""

        -- The complexity related to `compopt -o nospace` is so that we do not
        -- insert space when the only completion is in the form of `--option=`
        -- or `/some/path/`.
        --
        -- The stuff with `${COMP_WORDBREAKS//:}` and `compopt +o filenames` is
        -- to be able to complete URLs, e.g. `--url=http://localhost`, without
        -- Bash interpreting it differently.
        in  ''
            function _${name}()
            {
                COMP_WORDBREAKS="''${COMP_WORDBREAKS//=}"
                COMP_WORDBREAKS="''${COMP_WORDBREAKS//:}"
                local IFS=$'\n'
                mapfile -t COMPREPLY < <(
                    COMMAND_WRAPPER_INVOKE_AS='${toolset}' "${command}" --verbosity=silent \
                        completion \
                        --index="''${COMP_CWORD}" ${subcommandOption}\
                        --shell=bash \
                        -- "''${COMP_WORDS[@]}"
                )
                compopt +o filenames
                if (( ''${#COMPREPLY[@]} == 1 )); then
                    if [[ "''${COMPREPLY[0]}" == "''${COMPREPLY[0]%=}=" \
                       || "''${COMPREPLY[0]}" == */ \
                       ]]
                    then
                        compopt -o nospace
                    fi
                else
                    compopt -o nospace
                fi
            }

            ${completeNames}
            ''

let fishTemplate =
        λ(name : Text)
      → λ(toolset : Text)
      → λ(command : Text)
      → λ(subcommand : Optional Text)
      → λ(names : List Text)
      → let subcommandOption =
              Optional/fold
                Text
                subcommand
                Text
                (λ(cmd : Text) → "--subcommand='${cmd}' ")
                ""

        let complete =
                λ(cmd : Text)
              → "complete --no-files --command \"${cmd}\" --arguments '(_${name})'"

        let completeNames =
              List/fold
                Text
                names
                Text
                (λ(alias : Text) → λ(t : Text) → complete alias ++ "\n" ++ t)
                ""

        in  ''
            function _${name}
                set -l cl (commandline --tokenize --current-process)
                # Hack around fish issue #3934
                set -l cn (commandline --tokenize --cut-at-cursor --current-process)
                set -l cn (count $cn)
                set -l tmpline --index=$cn ${subcommandOption}--shell=fish -- $cl
                for opt in (env COMMAND_WRAPPER_INVOKE_AS='${toolset}' --verbosity=silent "${command}" completion $tmpline)
                  if test -d $opt
                    echo -E "$opt/"
                  else
                    echo -E "$opt"
                  end
                end
            end

            ${completeNames}
            ''

let zshTemplate =
        λ(name : Text)
      → λ(toolset : Text)
      → λ(command : Text)
      → λ(subcommand : Optional Text)
      → λ(names : List Text)
      → let subcommandOption =
              Optional/fold
                Text
                subcommand
                Text
                (   λ(cmd : Text)
                  → ''
                    \
                    ${"        "}--subcommand="${cmd}" ''
                )
                ""

        let complete = λ(cmd : Text) → "compdef '_${name}' '${cmd}'"

        let completeNames =
              List/fold
                Text
                names
                Text
                (λ(alias : Text) → λ(t : Text) → complete alias ++ "\n" ++ t)
                ""

        in  ''
            function _${name} {

            local completions
            local word
            local index=$((CURRENT - 1))

            IFS=$'\n' completions=($(
                COMMAND_WRAPPER_INVOKE_AS='${toolset}' "${command}" --verbosity=silent \
                    completion \
                    --index="''${index}" ${subcommandOption}\
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
            }

            ${completeNames}
            ''

in    λ(shell : < Bash | Fish | Zsh >)
    → λ(toolsetName : Text)
    → λ(executable : Text)
    → λ(subcommand : Optional Text)
    → λ(aliases : List Text)
    → let name =
            Optional/fold
              Text
              subcommand
              Text
              (λ(cmd : Text) → "${toolsetName}_${cmd}")
              toolsetName

      let names =
            Optional/fold
              Text
              subcommand
              (List Text)
              (λ(_ : Text) → aliases)
              ([ toolsetName ] # aliases)

      in  merge
            { Bash = bashTemplate name toolsetName executable subcommand names
            , Fish = fishTemplate name toolsetName executable subcommand names
            , Zsh = zshTemplate name toolsetName executable subcommand names
            }
            shell
