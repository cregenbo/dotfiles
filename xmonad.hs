import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Layout.Tabbed
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Spacing
import XMonad.Util.SpawnOnce
import XMonad.Util.Dmenu
import XMonad.Hooks.DynamicLog

import qualified Data.Map as M

myLayout = spacingRaw True (Border 0 0 0 0) False (Border 7 7 7 7) True $ Tall 1 (3/100) (1/2) ||| simpleTabbed

myStartup:: X ()
myStartup = do
  spawnOnce "feh --bg-fill ~/Pictures/marcelo-cidrack-HME4dq3FCeI-unsplash.jpg"
  spawnOnce "emacs --daemon"
  spawnOnce "picom --config ~/.config/picom/picom.conf"

main :: IO ()
main = xmonad =<<  myXmobar myConfig

myXmobar = statusBar "xmobar ~/.xmonad/xmobar.hs" myXmobarPP toggleStrutsKey
myXmobarPP = xmobarPP {ppTitle   = xmobarColor "green"  "" . shorten 120, ppLayout = const ""}
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

myConfig = ewmh $ def
  { modMask     = mod4Mask
  , terminal    = "kitty"
  , layoutHook  = myLayout
  , startupHook = myStartup
  } `additionalKeysP` myKeyBindings

myKeyBindings :: [(String, X())]
myKeyBindings =
  [ ("M-w", configFilesDmenu)
  ]

configFiles :: M.Map String String
configFiles = M.fromList
  [ ("xmonad", ".xmonad/xmonad.hs")
  , ("zsh", ".zshrc")
  , ("kitty", ".config/kitty/kitty.conf")
  , ("picom", ".config/picom/picom.conf")
  ]

configFilesDmenu :: X ()
configFilesDmenu =  do
  selection <- dmenuMap configFiles
  whenJust selection (\s -> spawn ("emacsclient -c ~/" ++ s))
