#!/bin/sh

# sleep 0.2 # give time for scrot to grab keyboard
import /tmp/area-screenshot.png
geturl /tmp/area-screenshot.png
notify-send -i ~/.xmonad/scripts/screenshot.png 'Uploaded screenshot' 'of area to filepicker.io'
