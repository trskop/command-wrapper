#plugins+=(zsh-completions)
autoload -Uz compinit promptinit
compinit

promptinit
prompt redhat

_load_cw_dev_completion() {
  if [[ -n ${CW_DEV_ZSH_COMPLETION} ]]; then
    source "${CW_DEV_ZSH_COMPLETION}"

    # Aliases and completion for them can be introduced here as well:
    alias hb=cw_dev
    compdef '_cw-dev' 'hb'
  else 

    alias hb &>/dev/null && unalias hb
    [[ -n $_comps[hb] ]] && compdef -d 'hb'
    [[ -n $_comps[cw-dev] ]] && compdef -d 'cw-dev'
  fi
}
typeset -ag precmd_functions;
if [[ -z ${precmd_functions[(r)_load_cw_dev_completion]} ]]; then
  precmd_functions+=_load_cw_dev_completion;
fi
