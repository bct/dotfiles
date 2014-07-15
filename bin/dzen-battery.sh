#!/bin/sh
# Time-stamp: <2008-08-24 21:31:03 fred>
#
# originally by lyon8 (lyon8@gmx.net)
# show your laptop battery state in dzen

BATFOLDER='/sys/class/power_supply/BAT0' # battery's info folder

GFG='#333'  # color of the gauge
GH=7       # height of the gauge
GBG='#999'  # color of gauge background
GW=50      # width of the gauge
LOWBAT=25        # percentage of battery life marked as low
LOWCOL='#ff4747' # color when battery is low
TIME_INT=10      # time interval in seconds

# The following variables should be inherited from the calling
# environment, but set here 'just in case'
BG=${BG:-'grey'}  # dzen backgrounad
FG=${FG:-'black'}  # dzen foreground
BAT_BEGIN=${BAT_BEGIN:-800}
BAT_W=${BAT_W:-170}     # width of the dzen bar
ICONPATH=${ICONPATH:-'/donnees/images/icons'}

FN=${FN:-'-*-clean-*-*-*-*-*-120-*-*-*-*-iso8859-*'} # font

while true; do
    sleep $TIME_INT;
    # look up battery's data
    BAT_FULL=`cat $BATFOLDER/energy_full`
    STATUS=`cat $BATFOLDER/status`
    RCAP=`cat $BATFOLDER/energy_now`

    # calculate remaining power
    RPERCT=$(( $RCAP * 100 ));
    RPERC=$(( $RPERCT / $BAT_FULL ));

    # draw the bar and pipe everything into dzen

    # color the gauge at low charge
    GFG_=$GFG
    [ $RPERC -le $LOWBAT ] && GFG_=$LOWCOL
    # RPERC is somtimes > 100 when the battery is almost full
    [ $RPERC -le 100 ] || RPERC=100

    case $STATUS in
	Discharging )
      # compute the remaining time (in minutes) we have
	    RATE=`cat $BATFOLDER/power_now`
      TIMELFT=$(echo $(( (60 * $RCAP) / $RATE )))

	    echo -n " ^i(${ICONPATH}/battery.xbm)"
	    echo $RPERC | dzen2-gdbar -h $GH -w $GW -fg $GFG_ -bg $GBG -nonl
	    echo " (${TIMELFT} min)"
	    ;;
	Charging )
	    echo -n " ^i(${ICONPATH}/power-ac.xbm)"
	    echo $RPERC | dzen2-gdbar -h $GH -w $GW -fg $GFG_ -bg $GBG -nonl
	    echo " (charging)"
	    ;;
	Full )
	    echo -n " ^i(${ICONPATH}/power-ac.xbm)"
	    echo $RPERC | dzen2-gdbar -h $GH -w $GW -fg $GFG_ -bg $GBG -nonl
	    echo " (full)"
	    ;;
	* )
	    echo -n " ??? $STATUS" ;;
    esac
done | dzen2 -ta l -tw $BAT_W -x $BAT_BEGIN -fg $FG -bg $BG -fn $FN
