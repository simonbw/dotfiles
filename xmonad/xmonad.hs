import Data.List
import Data.Map (fromList)
import Data.Monoid
import Data.Monoid (mappend)
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.Named
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger
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
myWorkspaces :: [String]
myWorkspaces    = clickable . (map dzenEscape) $ ["1","2","3","4","5","6","7","8","9"]
    --where clickable l = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" | (i,ws) <- zip [1..] l, let n = i ]
    where clickable l   = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" | (i,ws) <- zip [1..] l, let n = i ]

-- Borders
myBorderWidth        = 1
myNormalBorderColor  = "#333"
myFocusedBorderColor = "#CCC"


-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 	[
    -- launch a terminal
      ((modm .|. shiftMask,                 xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    , ((modm,                               xK_p     ), spawn "exe=`dmenu_run` && eval \"exec $exe\"")
    -- launch gmrun
    , ((modm .|. shiftMask,                 xK_p     ), spawn "gmrun")
    -- close focused window
    , ((modm .|. shiftMask,                 xK_c     ), kill)
     -- Rotate through the available layout algorithms
    , ((modm,                               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask,                 xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,                               xK_n     ), refresh)
    -- Move focus to the next window
    , ((modm,                               xK_j     ), windows W.focusDown)
    , ((modm,                               xK_Tab   ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,                               xK_k     ), windows W.focusUp  )
    , ((modm .|. shiftMask,                 xK_Tab   ), windows W.focusUp)
    -- Move focus to the master window
    , ((modm,                               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,                               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask,                 xK_j     ), windows W.swapDown  )
    , ((modm .|. controlMask,               xK_Tab   ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask,                 xK_k     ), windows W.swapUp  )
    , ((modm .|. shiftMask .|. controlMask, xK_Tab   ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,                               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,                               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,                               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm,                               xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm,                               xK_period), sendMessage (IncMasterN (-1)))

    -- launch programs
    --, ((modm,                               xK_i     ), spawn "~/idea-IU-135.690/bin/idea.sh")
    , ((modm,                               xK_i     ), spawn "idea&")
    , ((modm,                               xK_o     ), spawn "subl")
    , ((modm .|. shiftMask,                 xK_z     ), spawn "gnome-screensaver-command -l")

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

    -- show grid select thing
     , ((modm, xK_g), goToSelected defaultGSConfig)
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "killall xfce4-panel volti stalonetray trayer conky dzen2; xmonad --recompile; xmonad --restart")
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
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
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
    delta   = 5/100
    -- Default proportion of screen occupied by master pane
    ratio   = 3/4


-- Window rules:
myManageHook = manageHook defaultConfig <+> manageDocks
 

-- Event handling
myEventHook = docksEventHook
 

-- Startup hook
myStartupHook = setWMName "LG3D"


-- Status bars and logging
screen1width = 1920
screen2width = 1920
trayerWidth = 100
conkyWidth = 910 --(screen2width - trayerWidth) `div` 2
dzenFont = "-fn '-misc-fixed-medium-r-normal--14-*-*-*-*-*-*-*'"
--dzenFont = "-fn '-bitstream-bitstream charter-medium-r-normal--17-*-*-*-*-*-*-*'"
resetDislay = "xrandr --output DP1 --auto --left-of DP2"
myXmonadBar = "dzen2 -x '" ++ show(screen1width) ++ "' -y '0' -h '20' -w '" ++ show(screen2width - conkyWidth - trayerWidth) ++ "' -ta 'l' -fg '#FFF' -bg '#111' -e 'button2=;' " ++ dzenFont
myConkyBar = "conky -c ~/.xmonad/conky_dzen | dzen2 -x '" ++ show(screen1width + screen2width - conkyWidth) ++ "' -w '" ++ show(conkyWidth) ++ "' -h '20' -ta 'r' -bg '#111' -fg '#FFFFFF' -y '0' -e 'button2=;' " ++ dzenFont
trayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand false --widthtype pixel --width " ++ show(trayerWidth) ++ " --height 20 --distance -" ++ show(screen2width - conkyWidth) ++ " --distancefrom right --tint 0x111111 --transparent true --alpha 0"
volti = "volti"

myLogHook :: Handle -> X ()
myLogHook h = takeTopFocus <+> dynamicLogWithPP dzenPP
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
    spawnPipe trayer
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myConkyBar
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