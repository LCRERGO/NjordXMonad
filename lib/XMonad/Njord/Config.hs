--     _   __  _                __ 
--    / | / / (_)___  _________/ / 
--   /  |/ / / / __ \/ ___/ __  /  
--  / /|  / / / /_/ / /  / /_/ /   
-- /_/ |_/_/ /\____/_/   \__,_/    
--      /___/                      
--       
-- Author: Lucas Cruz dos Reis(L.C.R.) 
-- Github: https://github.com/LCRERGO 

module XMonad.Njord.Config
    ( njordConfig )
    where
    
-- XMonad Base
import XMonad

-- XMonad Configs
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.ManageDocks

-- XMonad Hooks
import XMonad.Hooks.DynamicLog

-- XMonad Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.PolybarFormating

-- Njord Configuration
import qualified XMonad.Njord.Applications as N
import qualified XMonad.Njord.Colors as N
import qualified XMonad.Njord.Keybindings as N
import qualified XMonad.Njord.Layouts as N
import qualified XMonad.Njord.Misc as N
import qualified XMonad.Njord.Prompt as N

njordConfig barProc = desktopConfig
        { terminal = N.njordTerminal
        , handleEventHook = docksEventHook
        , modMask = N.njordModMask
        , layoutHook = N.njordLayoutHook
        , startupHook = N.njordStartupHook
        , workspaces = N.polybarWorkspaces
        , logHook = dynamicLogWithPP (polybarLogHook barProc)
        } `additionalKeysP` N.njordKeys

polybarLogHook dbus = def 
    { ppCurrent = polybarColor N.purple3 ""
    , ppUrgent  = polybarColor N.red1 ""
    , ppTitle   = const ""
    , ppOutput  = N.dbusOutput dbus
    }
