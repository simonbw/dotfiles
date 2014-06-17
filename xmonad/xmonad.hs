import Data.Map (fromList)
import Data.Monoid
import Data.Monoid (mappend)
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.Named
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Util.Dzen
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- default terminal to use
myTerminal      = "gnome-terminal"
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- remap mod key to super key
myModMask       = mod4Mask
 
-- names of the default workspaces
myWorkspaces    = clickable . (map dzenEscape) $ ["1","2","3","4","5","6","7"]
    where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]
-- Borders
myBorderWidth        = 1
myNormalBorderColor  = "#666"
myFocusedBorderColor = "#FFF"


-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 	[
    -- launch a terminal
      ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_run` && eval \"exec $exe\"")
    -- launch gmrun
    --, ((modm .|. shiftMask, xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- volume controlFF
    , ((0                 , 0x1008FF11), spawn "amixer -q set Master playback 1-")
    , ((0                 , 0x1008FF13), spawn "amixer -q set Master playback 1+; amixer -q set Master playback unmute; amixer -q set Headphone unmute; amixer -q set Speaker unmute; amixer -q set PCM unmute")
    , ((0                 , 0x1008FF12), spawn "amixer -q set Master playback toggle; amixer -q set Headphone unmute; amixer -q set Speaker unmute; amixer -q set PCM unmute")
    --, ((0                 , 0x1008FF11), lowerVolume 2 >>= alert)
    --, ((0                 , 0x1008FF13), raiseVolume 2 >>= alert)
    --, ((0                 , 0x1008FF12), toggleMute >> return ())

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
     , ((modm              , xK_b     ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "killall xfce4-panelvolti stalonetray trayer conky dzen2; xmonad --recompile; xmonad --restart")
    ]
    ++
 
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
 
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]


-- Layouts:
myLayout = avoidStruts tiled ||| avoidStruts (named "Wide" (Mirror tiled)) ||| noBorders (fullscreenFloat Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1 
    -- Percent of screen to increment by when resizing panes
    delta   = 2/100
    -- Default proportion of screen occupied by master pane
    ratio   = 3/4


-- Window rules:
myManageHook = manageHook defaultConfig <+> manageDocks
 

-- Event handling
myEventHook = docksEventHook
 

-- Startup hook
myStartupHook = startupHook defaultConfig


-- Status bars and logging
resetDislay = "xrandr --output DFP2 --auto --right-of CRT1"
myXmonadBar = "dzen2 -x '1280' -y '0' -h '20' -w '1000' -ta 'l' -fg '#FFF' -bg '#111' -fn '-11' -e 'button2=;'"
myStatusBar = "conky -c /home/simon/.xmonad/conky_dzen | dzen2 -x '2480' -w '400' -h '20' -ta 'r' -bg '#111' -fg '#FFFFFF' -y '0' -fn '-11' -e 'button2=;'"
xfceBar = "xfce4-panel -d "
trayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype pixel --height 20 --width 200 --distance -1200 --distancefrom right --tint 0x111111 --transparent true --alpha 0"
volti = "volti"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ dzenPP
    {
        ppCurrent           =   dzenColor "#EBAC54" "#444" . pad
      , ppVisible           =   dzenColor "#FFF" "#444" . pad
      , ppHidden            =   dzenColor "#FFF" "#222" . pad
      , ppHiddenNoWindows   =   dzenColor "#7B7B7B" "#222" . pad
      , ppUrgent            =   dzenColor "#FF0000" "#222" . pad
      , ppWsSep             =   " "
      , ppSep               =   "    "
      , ppLayout            =   dzenColor "#FFF" "#333" . dzenEscape . pad . pad
      , ppTitle             =   (" " ++) . dzenColor "#FFF" "#111" . dzenEscape . pad
      , ppOutput            =   hPutStrLn h
    }


-- run xmonad
main = do
    spawn resetDislay
    spawn volti
    --spawn xfceBar
    spawnPipe trayer
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- input bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook dzenLeftBar,
        startupHook        = myStartupHook
    }