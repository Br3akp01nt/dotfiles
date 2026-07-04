{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Bifunctor                 (Bifunctor (first))
import           Data.Ratio                     ((%))
import qualified Data.Time.Clock                as C
import           XMonad                         hiding (restart)
import           XMonad.Br3akp01nt.Programs
import           XMonad.Layout.Accordion
import           XMonad.Layout.CircleEx
import           XMonad.Layout.Combo            (combineTwo)
import           XMonad.Layout.ComboP           (SwapWindow (..))
import qualified XMonad.Layout.Spacing          as SPC
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane          (TwoPane (TwoPane))
import           XMonad.Layout.WindowNavigation
import           XMonad.Util.EZConfig           (additionalKeys,
                                                 additionalKeysP)


modmask = mod1Mask

main :: IO ()
main = xmonad
     $ def
     { layoutHook = SPC.spacingWithEdge 15 $ windowNavigation myLayout
     , modMask = modmask
     , terminal = "xfce4-terminal -e tmux"
     , normalBorderColor = "#185c1f"
     , focusedBorderColor = "#58b8a5"
     , focusFollowsMouse = False
     , clickJustFocuses = True
     , startupHook = "XMonad started" `dzen` 4
     } `additionalKeys` keybinds

myLayout =
    combineTwo (TwoPane (1 % 10) (1 % 4)) Accordion (Mirror Accordion)
    ||| circleLayout
    ||| Full
    ||| Accordion

circleLayout = circleEx 
  { cNMaster = 1
  , cStackRatio = toRational $ 1 / pi
  , cMultiplier = 1
  , cDelta = pi / 2
  }

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
restart = spawn $ unlines
  [ "if type xmonad;"
  , "  then xmonad --recompile && xmonad --restart;"
  , "  else xmessage xmonad not in \\$PATH: \"$PATH\";"
  , "fi"
  ]

dzen :: String -> Int -> X ()
dzen m t = spawn $ "echo '" <> m <> "' | dzen2 -p " <> show t

