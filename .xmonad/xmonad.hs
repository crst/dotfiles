import XMonad

import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

import XMonad.Actions.UpdatePointer

import XMonad.Layout.NoBorders
import XMonad.Layout.Grid

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName


myScratchPads = [NS "konsole" "urxvt -title Konsole -e tmux" (title =? "Konsole") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
                 NS "scratchpad" "emacs -title Scratchpad" (title =? "Scratchpad") (customFloating $ W.RationalRect (1/4) (1/4) (1/3) (1/3)),
                 NS "sound" "urxvt -title Sound -e alsamixer" (title =? "Sound") (customFloating $ W.RationalRect (2/3) (2/3) (1/3) (1/3))]


myLayoutHook = (Tall 1 (3/100) (2/3) ||| Grid ||| noBorders Full)


myLogHook = updatePointer (Relative 0.5 0.5)


            -- http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#Screens_are_in_wrong_order
myKeys = [(mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action)) | (key, scr)  <- zip "wer" [1,0,2]
                                                                                            , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]]
         ++
         [("M-x M-c", spawn "xscreensaver-command -lock")]
         ++
         [("M-i", namedScratchpadAction myScratchPads "konsole"),
          ("M-O", namedScratchpadAction myScratchPads "scratchpad"),
          ("M-d", namedScratchpadAction myScratchPads "sound")]


main = do
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig <+> namedScratchpadManageHook myScratchPads
    , layoutHook = avoidStruts $ myLayoutHook
    , logHook = myLogHook
    , modMask = mod4Mask
    , startupHook = setWMName "LG3D"
    , terminal = "urxvt"
    , focusedBorderColor = "#228b22"
    } `additionalKeysP` myKeys