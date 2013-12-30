
xrandr --output DVI-0 --auto --primary --output VGA-0 --auto --left-of DVI-0


setxkbmap -layout us
xmodmap ~/.Xmodmap

xscreensaver -no-splash &
xsetroot -cursor_name left_ptr
xsetroot -solid black
redshift -l 52.31:13.24 -t 5700:3600 -g 0.8 -m randr -v &

stalonetray &
xmobar &

nm-applet --sm-disable &
batti &


exec zsh -c "udiskie & xmonad"
