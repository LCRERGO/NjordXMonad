--     _   __  _                __ 
--    / | / / (_)___  _________/ / 
--   /  |/ / / / __ \/ ___/ __  /  
--  / /|  / / / /_/ / /  / /_/ /   
-- /_/ |_/_/ /\____/_/   \__,_/    
--      /___/                      
--       
-- Author: Lucas Cruz dos Reis(L.C.R.) 
-- Github: https://github.com/LCRERGO 

-- Njord XMonad configuration file

-- XMonad Base
import XMonad

-- XMonad Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

-- XMonad Utils
import XMonad.Util.Run (safeSpawn, spawnPipe)

-- Njord Configuration
import qualified XMonad.Njord.Config as N
import qualified XMonad.Njord.Misc as N


main :: IO ()
main = do
    barProc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
    xmonad . ewmh . docks $ N.njordConfig barProc
