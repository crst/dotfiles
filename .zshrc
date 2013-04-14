autoload -U compinit promptinit
compinit
promptinit

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle :compinstall filename '$HOME/.zshrc'

prompt adam2

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep
bindkey -e
PATH=~/bin:$PATH


export PAGER=less
export TERM=xterm-256color
export EDITOR="emacs -nw"
