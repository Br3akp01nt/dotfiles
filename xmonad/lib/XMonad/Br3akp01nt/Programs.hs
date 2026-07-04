module XMonad.Br3akp01nt.Programs (lockscreen) where
import           XMonad      (X)
import           XMonad.Core (spawn)

lockscreen :: X ()
lockscreen = spawn "betterlockscreen -l"

