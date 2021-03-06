# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="kikusimple"
#ZSH_THEME="kiku"

#plugins=(git)

source $ZSH/oh-my-zsh.sh

stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments

# History in cache directory:
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=~/.zsh_history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# vi mode
#bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^w' backward-delete-word # Delete word binding fix after using j or k in vim mode
bindkey '^R' history-incremental-search-backward

# Yank to the system clipboard
function vi-yank-xclip {
    zle vi-yank
    echo "$CUTBUFFER" | xclip -in -selection clipboard
}

zle -N vi-yank-xclip
bindkey -M vicmd 'y' vi-yank-xclip

export FZF_DEFAULT_COMMAND="find -L -maxdepth 4"
export PAGER="less"
export TERMINAL="st"
export EDITOR="nvim"
export GOBIN="$HOME/code/go/bin"
export GOPATH="$HOME/code/go"

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

alias logout='loginctl kill-session $XDG_SESSION_ID'
alias record='ffmpeg -video_size 1920x1080 -framerate 30 -f x11grab -i :0.0+0,0 -c:v libx264rgb -crf 0 -preset ultrafast'
alias copy='xclip -selection clipboard'
alias hst="history 1 -1 | cut -c 8- | uniq | fzf | tr -d '\n' | xclip -sel c"
alias ansi="ansible-playbook $HOME/.ansible_dotfiles_installer/local.yml"
alias e="emacs &"
alias rw="rlwrap --always-readline"

# start tmux in st or urxvt
if [[ $- != *i* ]]; then
    return
elif [[  $TERM == "rxvt-"* || $TERM == "st-"* || $TERM == "xterm-256color" && -z "$TMUX" ]]; then
    exec tmux new-session -A
fi

[[ -r "/usr/share/z/z.sh" ]] && source /usr/share/z/z.sh

# install and initialize autopair pluggin
if [[ ! -d ~/.zsh-autopair ]]; then
  git clone https://github.com/hlissner/zsh-autopair ~/.zsh-autopair
fi

source ~/.zsh-autopair/autopair.zsh
autopair-init
