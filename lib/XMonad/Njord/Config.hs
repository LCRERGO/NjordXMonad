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

-- Additional Haskell Imports
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import System.IO (hPutStrLn)

-- Njord Configuration
import qualified XMonad.Njord.Applications as N
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
        , workspaces = N.xmobarWorkspaces
        , logHook = dynamicLogWithPP (xmobarLogHook barProc)
        } `additionalKeysP` N.njordKeys

xmobarLogHook proc = xmobarPP
    { ppOutput = hPutStrLn proc
    , ppTitle = const ""
    , ppWsSep = " "
    }
