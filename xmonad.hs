import XMonad
import System.Exit
import System.IO
{-import XMonad.Actions.KeyRemap-}
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run

import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/.xmobarrc"
  spawn "$HOME/.xmonad/autostart.sh"
  xmonad $ defaultConfig
    { terminal    = "sakura"
    , borderWidth = 3
    , focusedBorderColor = "#009900"
    , modMask     = myModMask
    {-, startupHook = myStartupHook-}
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    }
    , workspaces = myWorkspaces
    , focusFollowsMouse = myFocusFollowsMouse
    , keys = myKeys
    }

myModMask = mod4Mask
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_r     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

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
    , ((modm              , xK_b     ), shiftToNext)

    -- Move window to prev workspace
    , ((modm .|. shiftMask, xK_b     ), shiftToPrev)

    -- Shrink the master area
    , ((modm .|. shiftMask, xK_d     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm .|. shiftMask, xK_n     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_y     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Remap keys
    {-, ((modm , xK_F1), setKeyRemap emptyKeyRemap)-}
    {-, ((modm , xK_F2), setKeyRemap dvorakProgrammerKeyRemap)-}
    ]
    ++

    --
    -- mod-[F1..F12], Switch to workspace N
    --
    -- mod-[F1..F12], Switch to workspace N
    -- mod-shift-[F1..F12], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{a,o,e}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,o,e}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_o, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    {-++ buildKeyRemapBindings [dvorakProgrammerKeyRemap, emptyKeyRemap]-}


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myManageHook = composeAll
    [ className =? "MPlayer"   --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

{-myStartupHook = do-}
  {-setDefaultKeyRemap emptyKeyRemap [dvorakProgrammerKeyRemap, emptyKeyRemap]-}
