import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.Spacing
-- import Xmonad.Util.SpawnOnce
import XMonad.Prompt as P
import XMonad.Prompt.Man
import XMonad.Util.NamedScratchpad
import XMonad.Actions.GridSelect

myLauncher = "rofi -show run -theme arthur"
myTerminal = "termite"
myModMask = mod4Mask
myLayoutHook = Tall 1 (3/100) (1/2) ||| Full
myWorkspaces = map show [1..9]
-- myStartupHook = do
--   spawnOnce "firefox"
--   spawnOnce "emacs"

myKeys = [
  ("M-p", spawn myLauncher),
  ("M-m", namedScratchpadAction scratchpads "monitor"),
  ("M-t", namedScratchpadAction scratchpads "term"),
  ("M-r", namedScratchpadAction scratchpads "ranger"),
  -- ("M-m", manPrompt P.def),
  ("M-g", goToSelected defaultGSConfig),
  ("M-S-s", spawn "systemctl suspend")
  ]

scratchpads = [ NS "monitor" (myTerminal ++ " -e htop") (title =? "htop") scratchFloat
              , NS "term" (myTerminal ++ " --title term") (title =? "term") scratchFloat
              , NS "ranger" (myTerminal ++ " -e ranger") (title =? "ranger") scratchFloat
              ]
  where scratchFloat = customFloating size
        size = W.RationalRect (1/6) (1/6) (2/3) (2/3)

main = xmonad $ def
  { terminal    = myTerminal
  , modMask     = myModMask
  , borderWidth = 0
  , layoutHook = smartSpacing 2 $ myLayoutHook
  , workspaces = myWorkspaces
  -- , logHook = myLogHook
  -- , startupHook = myStartupHook
  , manageHook = namedScratchpadManageHook scratchpads
  } `additionalKeysP` myKeys
