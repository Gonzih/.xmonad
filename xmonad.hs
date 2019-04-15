import XMonad
import System.Exit
import System.IO
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow(copy, kill1, copyToAll, killAllOtherCopies)
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Util.Run
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName
import XMonad.Actions.UpdatePointer
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import Control.Monad (liftM2)
-- import XMonad.Prompt (XPPosition(Top), XPConfig(..), defaultXPConfig)

main = do
  -- xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc"
  xmonad $ docks $ ewmh def
    -- { terminal    = "xfce4-terminal"
    { terminal    = "urxvt"
    , borderWidth = 2
    , focusedBorderColor = "#D60000"
    , modMask     = altMask
    , startupHook = myStartupHook >> setWMName "LG3D"
    , manageHook = manageHook kdeConfig <+> manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = smartSpacing 5 $ smartBorders (avoidStruts $ layoutHook defaultConfig)
    , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
    , logHook = updatePointer (0.5, 0.5) (0, 0)
                >> setWMName "LG3D"
                >> fadeInactiveLogHook 0.6
    , workspaces = myWorkspaces
    , focusFollowsMouse = myFocusFollowsMouse
    , mouseBindings = myMouseBindings
    , keys = myKeys
    }

superMask = mod4Mask
altMask = mod1Mask
myWorkspaces = ["1-misc", "2-music", "3-mail", "4-im", "5-kp", "6-irc", "7-fish", "8-fish", "9-fish", "0-www"]
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myRestartCmd = "xmonad --recompile; killall trayer; xmonad --restart"

myManageHook = composeAll $
    [ isFullscreen                    --> doFullFloat
    , className =? "Rhythmbox"        --> viewShift "2-music"
    , className =? "banshee"          --> viewShift "2-music"
    , className =? "Spotify"          --> viewShift "2-music"
    , className =? "Thunderbird"      --> viewShift "3-mail"
    , className =? "Mail"             --> viewShift "3-mail"
    , className =? "Skype"            --> viewShift "4-im"
    , className =? "TelegramDesktop"  --> viewShift "4-im"
    , className =? "Keybase"          --> viewShift "4-im"
    , className =? "keepassxc"        --> viewShift "5-kp"
    , className =? "yubioath-desktop" --> viewShift "5-kp"
    , className =? "Xchat"            --> viewShift "6-irc"
    , className =? "Slack"            --> viewShift "6-irc"
    , className =? "Firefox"          --> viewShift "0-www"
    , className =? "Google-chrome"    --> viewShift "1-misc"
    , className =? "Chromium"         --> viewShift "1-misc"
    , className =? "Kpackagekit"      --> unFloat
    ]

    ++ [className =? name --> doFloat  | name <- myFloats]
    ++ [className =? name --> doIgnore | name <- myIgnores]
  where myFloats = [ "MPlayer"
                   , "mplayer2"
                   , "mplayer"
                   , "vlc"
                   , "Vlc"
                   , "Y2base"
                   , "Vncviewer"
                   , "Gnuplot"
                   , "java-lang-Thread"
                   , "plasma"
                   , "Plasma"
                   , "plasma-desktop"
                   , "Plasma-desktop"
                   , "Klipper"
                   , "kmix"
                   , "Kmix"
                   , "krunner"
                   , "ksplashsimple"
                   , "ksplashqml"
                   , "ksplashshx"
                   , "yakuake"
                   , "Yakuake"
                   , "wrapper"
                   , "rofi"
                   , "plasmashell"
                   ]
        myIgnores  = [ "peksystray"
                     , "mate-panel"
                     , "kdesktop"
                     , "desktop_window"
                     , "Xfce4-notifyd"
                     ]
        viewShift = doF . liftM2 (.) W.view W.shift
        unFloat = ask >>= doF . W.sink

myStartupHook = do
  spawn "$HOME/.xmonad/autostart.sh"

myNumbersRow = [xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright, xK_parenleft, xK_equal, xK_asterisk, xK_parenright, xK_plus, xK_bracketright, xK_exclam]

------------------------------------------------------------------------
-- Prompt Config
--
-- myPromptConfig :: XPConfig
-- myPromptConfig = defaultXPConfig
--     { position          = Top
--     , promptBorderWidth = 0
--     , font              = "xft:inconsolata:size=12:antialias=true"
--     , height            = 24
--     , bgColor           = "black"
--     , fgColor           = "grey"
--     , bgHLight          = "#6B6382"
--     , fgHLight          = "#4A4459"
--     }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_r     ), spawn "rofi -show combi")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_r     ), spawn "gmrun")

--     -- copy window to all workspaces (make always visible)
--     , ((modm,               xK_j     ), windows copyToAll)

--     -- kill all other window copies (disable always visible)
--     , ((modm .|. shiftMask, xK_j     ),  killAllOtherCopies)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_x     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_h     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_t     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_h     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_t     ), windows W.swapUp    )

    -- Next Workspace
    , ((modm              , xK_n     ), nextWS)

    -- Prev Workspace
    , ((modm              , xK_d     ), prevWS)

    -- Move window to next workspace
    , ((modm .|. shiftMask, xK_n     ), shiftToNext)

    -- Move window to prev workspace
    , ((modm .|. shiftMask, xK_d     ), shiftToPrev)

    -- Shrink the master area
    , ((modm .|. shiftMask, xK_w     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm .|. shiftMask, xK_v     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm              , xK_f     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_w     ), sendMessage (IncMasterN (-1)))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_v     ), sendMessage (IncMasterN 1))

    -- close focused window
    , ((modm              , xK_q     ), kill1)

    -- Quit xmonad
    --, ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- , ((modm .|. shiftMask, xK_q     ), spawn "qdbus org.freedesktop.ScreenSaver /ScreenSaver Lock")

    -- Quit xmonad
    --, ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- , ((modm .|. shiftMask, xK_l     ), spawn "xfce4-session-logout")

    -- Restart xmonad
    , ((modm  .|. shiftMask, xK_p     ), spawn myRestartCmd)
    ]

    -- Programmer Dvorak
    -- mod-[1..9]       %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) myNumbersRow
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    -- Screenshot commands
    [ ((0, xK_Print), spawn "~/.xmonad/scripts/area-screenshot.sh")
    , ((modm, xK_Print), spawn "~/.xmonad/scripts/full-screenshot.sh") ]
    ++
    [ ((0, 0x1008FF2C), spawn "~/.xmonad/scripts/area-screenshot.sh")
    , ((modm, 0x1008FF2C), spawn "~/.xmonad/scripts/full-screenshot.sh") ]
    ++

    --
    -- mod-{; , .}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{; , .}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period, xK_semicolon] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++
    [
    -- win8 sequences sent by touch device Logitech T650
    -- one finger swipe from left edge
     --((mod4Mask .|. controlMask, xK_BackSpace ), nextWS)
    -- one finger swipe from right edge
    --, ((mod4Mask .|. mod1Mask, 0x1008ffb1), prevWS)
    -- one finger swipe from top edge
    --, ((mod4Mask .|. controlMask, 0x1008ffb1), spawn $ XMonad.terminal conf)
    -- three finger swipe up (sends super_r same as mod4)
    --, ((0, 0xffeb ), spawn "xmessage '3 up'")
    -- three finger swipe down
    --, ((mod4Mask, xK_d ),  spawn $ XMonad.terminal conf)
    ]

button6 :: Button
button6 = 6
button7 :: Button
button7 = 7
button8 :: Button
button8 = 8
button9 :: Button
button9 = 9
button10 :: Button
button10 = 10

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster)
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                         >> windows W.shiftMaster)

    -- T650 sends these for three finger swipes left and right
    --, ((0, button8), (\w -> focus w >> windows W.swapUp))
    --, ((0, button9), (\w -> focus w >> windows W.swapDown))
    , ((0, button8), (\w ->  screenWorkspace 0 >> focus w))
    , ((0, button9), (\w ->  screenWorkspace 1 >> focus w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
