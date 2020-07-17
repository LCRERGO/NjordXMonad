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

-- Njord Configuration
import qualified XMonad.Njord.Applications as N
import qualified XMonad.Njord.Keybindings as N
import qualified XMonad.Njord.Layouts as N
import qualified XMonad.Njord.Misc as N
import qualified XMonad.Njord.Prompt as N

njordConfig dbus = desktopConfig
        { terminal = N.njordTerminal
        , handleEventHook = docksEventHook
        , modMask = N.njordModMask
        , layoutHook = N.njordLayoutHook
        , startupHook = N.njordStartupHook
        , workspaces = N.polybarWorkspaces
        , logHook = dynamicLogWithPP (dbusLogHook dbus)
        } `additionalKeysP` N.njordKeys

-- colors
gray    = "#bfbfbf"
red     = "#ff4d4d"
blue    = "#2e9afe"
white   = "#eeeeee"
green   = "#8cdd08"


--------------------------------------------------------------------------------
-- Dbus helpers
--------------------------------------------------------------------------------

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

dbusLogHook :: D.Client -> PP
dbusLogHook dbus = def 
    { ppOutput          = dbusOutput dbus
    , ppCurrent         = polybarColor green ""
    , ppVisible         = polybarColor white ""
    , ppUrgent          = polybarColor red ""
    , ppHidden          = polybarColor blue ""
    , ppHiddenNoWindows = polybarColor gray ""
    , ppTitle           = const ""
    , ppWsSep           = " "
    , ppSep             = " | "
    }

polybarColor :: String -> String -> String -> String
polybarColor fg bg | null fg = wrap ("%{B" ++ bg ++ "} ") "%{B-}"
  | null bg = wrap ("%{F" ++ fg ++ "} ") "%{F-}"
  | not (null fg || null bg) = wrap ("%{F" ++ fg ++ "}" ++ 
    "%{B" ++ bg ++ "} ") "%{F- B-}"
  | otherwise = id
