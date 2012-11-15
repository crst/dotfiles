xrandr --output DVI-0 --auto --primary --output VGA-0 --auto --left-of DVI-0

xscreensaver -no-splash &

xmodmap ~/.Xmodmap

xsetroot -cursor_name left_ptr

xsetroot -solid black

redshift -l 53.33:10.0 -t 5700:3600 -g 0.8 -m randr -v &

eval $(gpg-agent --daemon) &

exec ck-launch-session zsh -c "udiskie & xmonad"
