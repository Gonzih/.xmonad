import XMonad
import System.Exit
import System.IO
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import Control.Monad (liftM2)
import XMonad.Prompt (XPPosition(Top), XPConfig(..), defaultXPConfig)

main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc"
  xmonad $ defaultConfig
    { terminal    = "gnome-terminal"
    , borderWidth = 3
    , focusedBorderColor = "#026396"
    , modMask     = myModMask
    , startupHook = myStartupHook >> setWMName "LG3D"
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = smartBorders (avoidStruts $ layoutHook defaultConfig)
    , handleEventHook = fullscreenEventHook
    , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 30
                    }
    , workspaces = myWorkspaces
    , focusFollowsMouse = myFocusFollowsMouse
    , keys = myKeys
    }

myModMask = mod4Mask
myWorkspaces = ["1-term", "2-web", "3-mail", "4-skype", "5-im", "6-irc", "7-zsh", "8-zsh", "9-zsh", "0-wrk"]
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myRestartCmd = "xmonad --recompile; killall trayer; xmonad --restart"

myManageHook = composeAll
    [ className =? "MPlayer"       --> doFloat
    , className =? "Vncviewer"     --> doFloat
    , isFullscreen                 --> (doF W.focusDown <+> doFullFloat)
    , className =? "Google-chrome" --> viewShift "2-web"
    , className =? "Thunderbird"   --> viewShift "3-mail"
    , className =? "Skype"         --> viewShift "4-skype"
    , className =? "Pidgin"        --> viewShift "5-im"
    , className =? "Empathy"       --> viewShift "5-im"
    , className =? "Xchat"         --> viewShift "6-irc"
    , className =? "Firefox"       --> viewShift "0-wrk"
    , className =? "Chromium"      --> viewShift "0-wrk"
    ]
  where viewShift = doF . liftM2 (.) W.greedyView W.shift

myStartupHook = do
  spawn "$HOME/.xmonad/autostart.sh"

myNumbersRow = [xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright, xK_parenleft, xK_equal, xK_asterisk, xK_parenright, xK_plus, xK_bracketright, xK_exclam]

------------------------------------------------------------------------
-- Prompt Config
--
myPromptConfig :: XPConfig
myPromptConfig = defaultXPConfig
    { position          = Top
    , promptBorderWidth = 0
    , font              = "xft:inconsolata:size=16:antialias=true"
    , height            = 24
    , bgColor           = "black"
    , fgColor           = "grey"
    , bgHLight          = "#6B6382"
    , fgHLight          = "#4A4459"
    }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_r     ), spawn "exe=`dmenu_run` && eval \"exec $exe\"")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_r     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

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
    , ((modm,               xK_Return), windows W.swapMaster)

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
    , ((modm              , xK_w ), sendMessage (IncMasterN (-1)))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_v), sendMessage (IncMasterN 1))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn myRestartCmd)

    -- Add workspace with prompt
    , ((modm              , xK_a     ), addWorkspacePrompt myPromptConfig)

    -- Rename workspace
    , ((modm              , xK_o     ), renameWorkspace myPromptConfig)

    -- Remove workspace
    , ((modm              , xK_e     ), removeEmptyWorkspace)

    ---- For dynamic workspaces
    -- Select workspace
    , ((modm              , xK_i     ), selectWorkspace myPromptConfig)

    -- Move client to workspace
    , ((modm              , xK_u     ), withWorkspace myPromptConfig (windows . W.shift))

    -- Copy client to workspace
    , ((modm .|. shiftMask, xK_u     ), withWorkspace myPromptConfig (windows . copy))
    ]

    -- Programmer Dvorak
    -- mod-[1..9]       %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    ++
    zip (zip (repeat (modm)) myNumbersRow) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modm .|. shiftMask)) myNumbersRow) (map (withNthWorkspace W.shift) [0..])
    ++

    -- Screenshot commands
    [ ((0, xK_Print), spawn "scrot /tmp/full-screenshot.png")
    , ((mod1Mask, xK_Print), spawn "scrot -b -s /tmp/area-screenshot.png") ]
    ++

    --
    -- mod-{a,o,e}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,o,e}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_semicolon, xK_comma, xK_period, xK_p, xK_y] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
