#!/usr/bin/sh

scrot /tmp/full-screenshot.jpeg
geturl /tmp/full-screenshot.jpeg
notify-send -i ~/.xmonad/scripts/screenshot.jpeg 'Uploaded screenshot' 'to filepicker.io'
