import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Layout.Tabbed
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Spacing
import XMonad.Util.SpawnOnce
import XMonad.Util.Dmenu
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CycleWS
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName
import XMonad.Actions.WindowBringer
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing

import qualified Data.Map as M

myLayout = smartBorders $ spacingRaw True (Border 0 0 0 0) False (Border 7 7 7 7) True $ Tall 1 (3/100) (1/2) ||| Full

myStartup:: X ()
myStartup = do
--  spawn "xrandr --output DisplayPort-0 --primary --set TearFree on"
--  spawn "xrandr --output HDMI-A-0 --same-as DisplayPort-0 --set TearFree on"
  spawnOnce "megasync"
  spawnOnce "warpd"
  spawnOnce "nitrogen --restore"
  setWMName "LG3D"

main :: IO ()
main = xmonad . myPolybar . ewmh . ewmhFullscreen $ myConfig

myPolybar = withEasySB (statusBarProp "polybar" (pure def)) defToggleStrutsKey

myConfig = def
  { modMask     = mod4Mask
  , terminal    = "kitty"
  , layoutHook  = myLayout
  , startupHook = myStartup
  , handleEventHook = myHandleEventHook
  } `additionalKeysP` myKeyBindings

myHandleEventHook = swallowEventHook (className =? "kitty") (return True)

myKeyBindings :: [(String, X())]
myKeyBindings =
  [ ("M-o", spawn "rofi -show window")
  , ("M-p", spawn "rofi -show drun")
  , ("M-s", spawn "rofi -show ssh")
  , ("M-`", spawn "maim --select ~/Pictures/screenshots/$(date +%s).png")
  , ("M-;", toggleWS)
  , ("M-e", spawn "emacsclient -c")
  , ("<XF86AudioMute>", spawn "amixer set Master toggle")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
  , ("M-S-s", spawn "systemctl suspend")
  ]
