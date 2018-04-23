import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.Spacing
import XMonad.Hooks.WallpaperSetter
-- import Xmonad.Util.SpawnOnce
import XMonad.Prompt as P
import XMonad.Prompt.Man

main = xmonad $ def
  { terminal    = myTerminal
  , modMask     = myModMask
  , borderWidth = 0
  , layoutHook = smartSpacing 2 $ myLayoutHook
  , workspaces = myWorkspaces
  , logHook = myLogHook
  -- , startupHook = myStartupHook
  } `additionalKeysP` myKeys

myLauncher = "rofi -show run -theme arthur"
myTerminal = "termite"
myModMask = mod4Mask
myLayoutHook = Tall 1 (3/100) (1/2) ||| Full
myWorkspaces = map show [1..9]
myLogHook = wallpaperSetter defWallpaperConf
-- myStartupHook = do
--   spawnOnce "firefox"
--   spawnOnce "emacs"

myKeys = [
  ("<F1>", spawn myTerminal),
  ("<F5>", spawn myLauncher),
  ("M-m", manPrompt P.def),
  ("M-S-s", spawn "systemctl suspend")
  ]
