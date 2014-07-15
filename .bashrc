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

set -o vi

# make ^L work
bind -m vi-insert 'Control-l: clear-screen'

# fuuuuuck you Firefox
[ -d ~/Desktop ] && rmdir ~/Desktop

export HISTCONTROL=ignoredups

. ~/.functions
