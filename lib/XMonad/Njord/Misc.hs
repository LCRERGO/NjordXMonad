--     _   __  _                __ 
--    / | / / (_)___  _________/ / 
--   /  |/ / / / __ \/ ___/ __  /  
--  / /|  / / / /_/ / /  / /_/ /   
-- /_/ |_/_/ /\____/_/   \__,_/    
--      /___/                      
--       
-- Author: Lucas Cruz dos Reis(L.C.R.) 
-- Github: https://github.com/LCRERGO 

module XMonad.Njord.Misc
    ( dbusOutput
    , njordWorkspaces
    , njordTerminal
    , njordModMask
    , njordStartupHook 
    , polybarWorkspaces
    )
    where

-- XMonad Base
import XMonad (KeyMask, WorkspaceId, X, spawn, mod4Mask)

-- XMonad Utilities
import XMonad.Util.SpawnOnce (spawnOnce)

-- Njord Configuration
import qualified XMonad.Njord.Applications as N

-- Haskell additional utilities
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

njordWorkspaces :: [WorkspaceId]
njordWorkspaces = 
    [ "\xf306" -- Debian
    , "\xf484" -- Web
    , "\xf670" -- Console
    , "\xf7b3" -- Games
    , "\xe22b" -- Art
    , "\xf040" -- Office
    , "\xf001" -- Music
    , "\xf7cd" -- Communication
    , "\xf6c3" -- Virtualization
    ]

polybarWorkspaces = clickable njordWorkspaces
    where clickable l = 
            [ "%{A1:xdotool key super+" ++ show i ++ ":}" ++ ws ++ " %{A}" | 
                (i, ws) <- zip [1..] l]

njordTerminal :: String
njordTerminal = N.terminal N.njordApplications

njordModMask :: KeyMask
njordModMask = mod4Mask

njordStartupHook :: X ()
njordStartupHook = do
    spawnOnce "dunst &"
    spawnOnce "compton -b"
    spawnOnce "set_wall"
    spawn "$XDG_CONFIG_HOME/polybar/launch.sh"
    spawnOnce "xsetroot -cursor_name left_ptr"

--------------------------------------------------------------------------------
-- Dbus signaler function
--------------------------------------------------------------------------------
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal xmonadObjPath xmonadIntName memberName)
                { D.signalBody = [D.toVariant $ UTF8.decodeString str]
                }
    D.emit dbus signal
        where 
            xmonadObjPath = D.objectPath_ "/org/xmonad/Log"
            xmonadIntName = D.interfaceName_ "org.xmonad.Log"
            memberName    = D.memberName_ "Update"
