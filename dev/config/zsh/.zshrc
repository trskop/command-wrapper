#plugins+=(zsh-completions)
autoload -Uz compinit promptinit
compinit

promptinit
prompt redhat

_load_project_toolset_completion() {
  if [[ -n ${HABIT_ZSH_COMPLETION} ]]; then
    source "${HABIT_ZSH_COMPLETION}"

    # Aliases and completion for them can be introduced here as well:
    alias hb=habit
    compdef '_habit' 'hb'
  else 

    alias hb &>/dev/null && unalias hb
    [[ -n $_comps[hb] ]] && compdef -d 'hb'
    [[ -n $_comps[habit] ]] && compdef -d 'habit'
  fi
}
typeset -ag precmd_functions;
if [[ -z ${precmd_functions[(r)_load_project_toolset_completion]} ]]; then
  precmd_functions+=_load_project_toolset_completion;
fi
