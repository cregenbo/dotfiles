import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.Spacing
-- import Xmonad.Util.SpawnOnce
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Prompt.Man
import XMonad.Util.NamedScratchpad
import XMonad.Layout.PerScreen
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import Control.Monad
import XMonad.Actions.TopicSpace
import Data.Map (fromList)
import XMonad.Hooks.SetWMName

myLauncher = "rofi -show run -theme DarkBlue"
myTerminal = "kitty"

myTerminalWithDir :: Dir -> String
myTerminalWithDir dir = myTerminal ++ " -d " ++ dir

myModMask = mod4Mask

myTopics :: [Topic]
myTopics =
  [ "dashboard"
  , "xmonad"
  , "java"
  , "anki"
  , "nixos"
  ]

myTopicConfig :: TopicConfig
myTopicConfig = def
  { topicDirs = fromList $
    [ ("dashboard", "~")
    , ("xmonad", "~/dotfiles")
    , ("java", "~")
    , ("anki", "~")
    , ("nixos", "/etc/nixos")
    ]
  , defaultTopic = "dashboard"
  , topicActions = fromList $
    [ ("dashboard", spawn "firefox -new-window www.google.com")
    , ("xmonad", spawn "gvim ~/dotfiles/xmonad.hs" >> spawnShell >> spawn "firefox -search \"xmonad hackage\"")
    , ("java", spawn "idea-ultimate" >> spawn "firefox -new-window www.google.com")
    , ("anki", spawn "anki")
    , ("nixos", spawnShellWith "sudo vim configuration.nix" >> spawn "firefox -new-window nixos.org" >> spawnShell)
    ]
  }

spawnShellWith :: String -> X ()
spawnShellWith cmd = do
  dir <- currentTopicDir myTopicConfig
  spawnShellInWith dir cmd

spawnShellInWith :: Dir -> String -> X ()
spawnShellInWith dir cmd = spawn $ myTerminalWithDir dir ++ " -- " ++ cmd

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminalWithDir dir

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

myLayoutHook = ifWider 1440 (ThreeColMid 1 (3/100) (1/2) ||| Tall 1 (3/100) (1/2) ||| Full) Full

myXPConfig :: XPConfig
myXPConfig = def
  { position = Top
  , font = "xft:Iosevka:style=Bold:pixelsize=20"
  , height = 40
  }

myKeys = 
  [ ("M-p", spawn myLauncher)
  , ("M-m", namedScratchpadAction scratchpads "monitor")
  , ("M-t", namedScratchpadAction scratchpads "term")
  , ("M-r", namedScratchpadAction scratchpads "ranger")
  , ("M-g", promptedGoto)
  , ("M-S-s", spawn "systemctl suspend")
  , ("M-S-<Return>", spawnShell)
  ]

scratchpads = [ NS "monitor" (myTerminal ++ " -e htop") (title =? "htop") scratchFloat
              , NS "term" (myTerminal ++ " --title term") (title =? "term") scratchFloat
              , NS "ranger" (myTerminal ++ " -e ranger") (title =? "ranger") scratchFloat
              ]
  where scratchFloat = customFloating size
        size = W.RationalRect (1/6) (1/6) (2/3) (2/3)

myConfig = do
  checkTopicConfig myTopics myTopicConfig
  return $ def
    { terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = 0
    , layoutHook = myLayoutHook
    , workspaces = myTopics
    -- , logHook = myLogHook
    , startupHook = setWMName "LG3D"
    --, manageHook = namedScratchpadManageHook scratchpads
    } `additionalKeysP` myKeys

main :: IO ()
main = xmonad =<< myConfig
