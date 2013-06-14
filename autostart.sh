#xsetroot -cursor_name left_ptr
#xsetroot -solid black
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true \
       --widthtype percent --width 15 --transparent true --tint 0x000000 \
       --heighttype pixel --height 25 --monitor primary &
#stalonetray --no-shrink --geometry 12x1+1630+0 --transparent false \
#            --background black --max-geometry 13x1  --icon-size 25 &
#xloadimage -onroot -fullscreen ~/Dropbox/Public/pics/wall/quokka-wall.jpg
