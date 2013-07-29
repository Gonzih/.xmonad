#!/usr/bin/sh

scrot -b -s /tmp/area-screenshot.png
geturl /tmp/area-screenshot.png
notify-send -i ~/.xmonad/scripts/screenshot.png 'Uploaded screenshot' 'to filepicker.io'
