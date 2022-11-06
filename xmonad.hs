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

import qualified Data.Map as M

myLayout = smartBorders $ spacingRaw True (Border 0 0 0 0) False (Border 7 7 7 7) True $ Tall 1 (3/100) (1/2) ||| Full

myStartup:: X ()
myStartup = do
  spawn "xrandr --output HDMI-A-0 --set TearFree on"
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
  } `additionalKeysP` myKeyBindings

myKeyBindings :: [(String, X())]
myKeyBindings =
  [ ("M-w", spawn "rofi -show window -font \"JetBrainsMono Nerd Font 20\"")
  , ("M-p", spawn "rofi -show drun -font \"JetBrainsMono Nerd Font 20\"")
  , ("M-s", spawn "rofi -show ssh -font \"JetBrainsMono Nerd Font 20\"")
  , ("M-;", toggleWS)
  , ("M-e", spawn "emacsclient -c")
  , ("M-S-s", spawn "systemctl suspend")
  ]

configFiles :: M.Map String String
configFiles = M.fromList
  [ ("xmonad", ".xmonad/xmonad.hs")
  , ("zsh", ".zshrc")
  , ("kitty", ".config/kitty/kitty.conf")
  ]

configFilesDmenu :: X ()
configFilesDmenu =  do
  selection <- dmenuMap configFiles
  whenJust selection (\s -> spawn ("emacsclient -c ~/" ++ s))
