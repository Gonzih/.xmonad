xsetroot -cursor_name left_ptr
xsetroot -solid black
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true \
        --widthtype percent --width 15 --transparent true --tint 0x000000 \
        --heighttype pixel --height 26 --monitor primary &
xloadimage -onroot -fullscreen ~/Dropbox/Public/pics/wall/quokka-wall.jpg
