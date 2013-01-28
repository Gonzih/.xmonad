#!/bin/sh

amixer get Capture | grep -E '\b(on|off)\b' | sed 's/.*\(on\|off\).*/\1/'
