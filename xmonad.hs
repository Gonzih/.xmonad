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


import qualified Data.Map as M
import qualified XMonad.StackSet as W

import Control.Monad (liftM2)
import XMonad.Prompt (XPPosition(Top), XPConfig(..), defaultXPConfig)

main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc"
  xmonad $ defaultConfig
    { terminal    = "sakura"
    , borderWidth = 3
    , focusedBorderColor = "#009900"
    , modMask     = myModMask
    , startupHook = myStartupHook
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = smartBorders (avoidStruts $ layoutHook defaultConfig)
    , handleEventHook = fullscreenEventHook
    , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    }
    , workspaces = myWorkspaces
    , focusFollowsMouse = myFocusFollowsMouse
    , keys = myKeys
    }

myModMask = mod4Mask
myWorkspaces = ["term", "web", "mail", "skype", "im", "code", "ff", "files", "9", "0"]
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myManageHook = composeAll
    [ className =? "MPlayer"       --> doFloat
    , className =? "Vncviewer"     --> doFloat
    , isFullscreen                 --> (doF W.focusDown <+> doFullFloat)
    , className =? "Thunderbird"   --> viewShift "mail"
    , className =? "Google-chrome" --> viewShift "web"
    , className =? "Chromium"      --> viewShift "web"
    , className =? "Firefox"       --> viewShift "ff"
    , className =? "Pidgin"        --> viewShift "im"
    , className =? "Skype"         --> viewShift "skype"
    , className =? "Gvim"          --> viewShift "code"
    , className =? "Nautilus"      --> viewShift "files"
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
    , ((modm .|. shiftMask, xK_Left  ), sendMessage Shrink)

    -- Expand the master area
    , ((modm .|. shiftMask, xK_Right ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_y     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN (-1)))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN 1))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Add workspace with prompt
    , ((modm              , xK_a     ), addWorkspacePrompt myPromptConfig)

    -- Rename workspace
    , ((modm              , xK_o     ), renameWorkspace myPromptConfig)

    -- Remove workspace
    , ((modm              , xK_e     ), removeEmptyWorkspace)

    ---- For dynamic workspaces
    -- Select workspace
    , ((modm    , xK_semicolon       ), selectWorkspace myPromptConfig)

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
    {-++-}

    --
    -- mod-{a,o,e}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,o,e}, Move client to screen 1, 2, or 3
    --
    {-[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))-}
        {-| (key, sc) <- zip [xK_a, xK_o, xK_e] [0..]-}
        {-, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]-}
