# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !

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

export HISTSIZE=10000
export HISTCONTROL=ignoredups
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

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

. /usr/share/autojump/autojump.sh

export PATH=/home/brendan/bin:/usr/local/bin:$PATH
. /home/brendan/.rbenvrc
. ~/.stripe-repos.sh
export PATH=/usr/local/bin:$PATH

source /home/brendan/.nix-profile/etc/profile.d/nix.sh

alias aoeu="setxkbmap us"
alias asdf="setxkbmap dvorak"

export LANG="en_GB.utf8"
export EDITOR=vim
export VTERM=urxvtc

alias cur-apiori="curl -sSf https://api.stripe.com/healthcheck | cut -d. -f1-2"
alias ssh=sc-ssh
alias mosh="mosh --ssh=sc-ssh"
source ~/stripe/space-commander/bin/sc-aliases

#if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
#  export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
#fi

SSH_ENV="$HOME/.ssh/env"

function start_agent {
  echo "Initialising new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  chmod 600 "${SSH_ENV}"
  . "${SSH_ENV}" > /dev/null
  /usr/bin/ssh-add

  # load up the SSH key used for github
  SSH_ASKPASS=/home/brendan/bin/git_ssh_pass ssh-add ~/.ssh/id_rsa_brendan@stripe.com </dev/null
}

# Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
  . "${SSH_ENV}" > /dev/null
  pgrep ssh-agent$ >/dev/null || start_agent
else
  start_agent
fi

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
