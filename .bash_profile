# /etc/skel/.bash_profile

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
[[ -f ~/.bashrc ]] && . ~/.bashrc

export STRIPE_USER=brendan

### BEGIN HENSON
export PATH="/home/bct/stripe/henson/bin:$PATH"
### END HENSON

export PATH="/home/bct/stripe/space-commander/bin:$PATH"
export PATH="/home/bct/stripe/password-vault/bin:$PATH"

export PATH="$HOME/.rbenv/bin:$PATH"

eval "$(rbenv init -)"
