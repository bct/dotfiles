eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg)
export GPG_AGENT_INFO

/usr/lib/notification-daemon/notification-daemon &
trayer --edge top --align right --width 20 --transparent true --height 20 &
nm-applet &

/home/bct/bin/dzen-battery.sh &

xfce4-power-manager

indicator-cpufreq &

setxkbmap dvorak
xmodmap ~/.xmodmaprc

exec xmonad
