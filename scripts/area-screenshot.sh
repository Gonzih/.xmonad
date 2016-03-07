#!/usr/bin/env fish

import /tmp/area-screenshot.png
dropbox-upload /tmp/area-screenshot.png
notify-send -i ~/.xmonad/scripts/screenshot.png 'Uploaded screenshot' 'of area to dropbox'
