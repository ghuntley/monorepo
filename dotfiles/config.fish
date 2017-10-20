# Configure classic prompt
set fish_color_user --bold blue
set fish_color_cwd --bold white

# Fish configuration
set fish_greeting ""
set PATH $HOME/.local/bin $GOPATH/bin $PATH

# Editor configuration
set -gx EDITOR "emacsclient"
set -gx ALTERNATE_EDITOR "emacs -q -nw"
set -gx VISUAL "emacsclient"

# Miscellaneous
eval (direnv hook fish)

# Useful command aliases
alias gpr 'git pull --rebase'
alias gco 'git checkout'
alias gf 'git fetch'
alias gap 'git add -p'
alias pbcopy 'xclip -selection clipboard'
alias edit 'emacsclient -n'

# Old habits die hard (also ls is just easier to type):
alias ls 'exa'
