Weather stations list http://weather.rap.ucar.edu/surface/stations.txt

Requirements:
* xmonad
* xmobar
* xcompmgr
* xfce4
* xfce4-popup-whiskermenu

Installation:
* Install haskell with cabal
* Init sandbox `cabal sandbox init` (optional)
* Run `cabal install xmonad --flags="with_utf8 with_xft"`
* Run `cabal install xmonad-contrib --flags="with_utf8 with_xft"`
* Run `cabal install xmobar --flags="with_utf8 with_xft"`
