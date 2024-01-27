{-# LANGUAGE TupleSections #-}

module Main where

import XMonad hiding (restart)
import qualified XMonad.Layout.Spacing as SPC
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.Combo (combineTwo)
import XMonad.Layout.TwoPane (TwoPane(TwoPane))
import Data.Ratio ((%))
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)
import XMonad.Layout.ComboP (SwapWindow(..))
import XMonad.Layout.Tabbed
import Data.Bifunctor (Bifunctor(first))
import qualified Data.Time.Clock as C
import XMonad.Br3akp01nt.Programs


modmask = mod1Mask

terminalProgram :: String
terminalProgram = "xfce4-terminal -e tmux"

main :: IO ()
main = xmonad 
     $ def 
     { layoutHook = SPC.spacingWithEdge 15 $ windowNavigation myLayout
     , modMask = modmask
     , terminal = terminalProgram
     , normalBorderColor = "#185c1f"
     , focusedBorderColor = "#58b8a5"
     , focusFollowsMouse = False
     , clickJustFocuses = True
     , startupHook = "XMonad started" `dzen` 4
     } `additionalKeys` keybinds

myLayout = 
    combineTwo (TwoPane (1 % 10) (1 % 4)) Accordion (Mirror Accordion)
    ||| Circle 
    ||| Full 
    ||| Accordion

keybinds :: [((KeyMask, KeySym), X ())]
keybinds =  
    [ ((modmask .|. shiftMask, xK_l), lockscreen) 
    , ((modmask, xK_q), "rebuilding and restarting xmonad" `dzen` 4 >> restart)
    ]
    <>
    map (first (modmask .|. controlMask .|. shiftMask, )) windowSwaps
  where
    windowSwaps = 
      [ (xK_Left,  sendMessage $ Move L)
      , (xK_Down,  sendMessage $ Move D)
      , (xK_Up,    sendMessage $ Move U)
      , (xK_Right, sendMessage $ Move R)
      , (xK_s,     sendMessage SwapWindow)
      ]

restart :: X ()
restart = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

dzen :: String -> Int -> X ()
dzen m t = spawn $ "echo '" <> m <> "' | dzen2 -p " <> show t

