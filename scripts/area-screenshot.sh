#!/usr/bin/sh

sleep 0.2 # give time for scrot to grab keyboard
scrot -s /tmp/area-screenshot.jpeg
geturl /tmp/area-screenshot.jpeg
notify-send -i ~/.xmonad/scripts/screenshot.jpeg 'Uploaded screenshot' 'to filepicker.io'
