#!/bin/sh

amixer get Capture | grep -E '\b(on|off)\b' | sed 's/.*\(on\|off\).*/\1/' | sed 's/on/<fc=#00ff00>+<\/fc>/' | sed 's/off/<fc=#ff0000>-<\/fc>/'
