xrandr --output DVI-0 --auto --primary --output VGA-0 --auto --left-of DVI-0

xscreensaver -no-splash &

xmodmap ~/.Xmodmap

xsetroot -cursor_name left_ptr

xsetroot -solid black

#redshift -l 53.33:10.0 -t 5700:3600 -g 0.8 -m randr -v &

trayer --edge top --align left --SetDockType true --SetPartialStrut true \
       --expand true --width 10 --transparent true --tint 0x333333 --height 12 &

if [ -x /usr/bin/nm-applet ] ; then
    nm-applet --sm-disable &
fi

exec zsh -c "udiskie & xmonad"
