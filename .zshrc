autoload -U compinit promptinit
compinit
promptinit

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle :compinstall filename '$HOME/.zshrc'

prompt adam2

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -e
PATH=~/bin:$PATH
