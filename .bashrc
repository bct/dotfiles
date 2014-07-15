# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !

alias aoeu="setxkbmap us"
alias asdf="setxkbmap dvorak"

export LANG="en_GB.utf8"
export RUBYOPT="-rubygems"
export PYTHONPATH="/usr/local/lib/python2.6/site-packages/"
export PATH="$PATH:/home/bct/bin"
export XDG_DATA_HOME=~/.local/share
export EDITOR=vim
export VTERM=urxvtc
#export VISUAL='urxvt -e vim'

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] $(__git_ps1 "(%s)")\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
  PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

set -o vi

# make ^L work
bind -m vi-insert 'Control-l: clear-screen'

export HISTCONTROL=ignoredups

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

[ -f ~/stripe/password-vault/bash_completion ] && . ~/stripe/password-vault/bash_completion


if [ -d ~/stripe/space-commander/bin/ ] && [ -d ~/.stripe/aws/ssh/ ]; then
  alias ssh="sc-ssh-wrapper"
fi

check ()
{
  ssh cave "cd /pay/deploy/checker/current; /usr/stripe/bin/stripe-bundle checker checker/bin/checker -r $1"
}

. /usr/share/autojump/autojump.sh
