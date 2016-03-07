#!/usr/bin/env fish

scrot /tmp/full-screenshot.png
dropbox-upload /tmp/full-screenshot.png
notify-send -i ~/.xmonad/scripts/screenshot.png 'Uploaded screenshot' 'of desktop to dropbox'
