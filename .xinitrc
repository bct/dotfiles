xmodmap ~/.xmodmaprc

rm /tmp/uzbl*

# add font paths (there's probably a better way but fuckit)
xset fp+ /usr/share/fonts/X11/liberation-fonts/

urxvtd &
urxvtc &

redshift -l 53:-113 &

# power management
sudo iwconfig wlan0 power timeout 500ms

uzbl-event-manager -s ~/.cache/uzbl/event_daemon_dirty start

# gotta sleep here so they'll start after the dzen2 instance below
(sleep 1; /home/bct/bin/mail-status) &
(sleep 1; /home/bct/bin/bank-status) &

exec /usr/bin/xmonad | dzen2 -h 12 -fn '-*-liberation serif-medium-r-normal--13-0-0-0-p-0-*'
#exec twm
