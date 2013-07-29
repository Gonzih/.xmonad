#!/usr/bin/sh

scrot /tmp/full-screenshot.png
geturl /tmp/full-screenshot.png
notify-send -i ~/.xmonad/scripts/screenshot.png 'Uploaded screenshot' 'to filepicker.io'
