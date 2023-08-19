{-# LANGUAGE TupleSections #-}

module Main where

import XMonad
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


myLayout = 
    Circle 
    ||| combineTwo (TwoPane (1 % 10) (1 % 4)) Accordion (tabbed shrinkText def)
    ||| Full 
    ||| Accordion

keybinds :: [((KeyMask, KeySym), X ())]
keybinds =  
    [ ((modmask .|. shiftMask, xK_l), spawn "betterlockscreen -l" >> spawn "echo 'locking' | dzen2 -p 4") 
    ]
    <>
    map (first (modmask .|. controlMask .|. shiftMask, )) windowSwaps
  where
    windowSwaps = 
      [ (xK_Left,  sendMessage (Move L))
      , (xK_Down,  sendMessage (Move D))
      , (xK_Up,    sendMessage (Move U))
      , (xK_Right, sendMessage (Move R))
      , (xK_s,     sendMessage SwapWindow)
      ]

modmask = mod1Mask

main :: IO ()
main = xmonad 
    $ def 
    { layoutHook = SPC.spacingWithEdge 15 $ windowNavigation myLayout
    , modMask = modmask
    , terminal = "xfce4-terminal"
    , normalBorderColor = "#185c1f"
    , focusedBorderColor = "#58b8a5"
    , focusFollowsMouse = False
    , clickJustFocuses = True
    , startupHook = spawn "echo 'XMonad started' | dzen2 -p 4"
    } `additionalKeys` keybinds
