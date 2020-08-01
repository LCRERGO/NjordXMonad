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
    ( xmobarWorkspaces
    , njordWorkspaces
    , njordTerminal
    , njordModMask
    , njordStartupHook )
    where

-- XMonad Base
import XMonad (KeyMask, WorkspaceId, X, spawn, mod4Mask)

-- XMonad Utilities
import XMonad.Util.SpawnOnce (spawnOnce)

-- Njord Configuration
import qualified XMonad.Njord.Applications as N

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

dzenWorkspaces = clickable njordWorkspaces
    where clickable l =
            [ "^ca(1, xdotool key super+" ++ show i ++ ")" ++ ws ++ "^ca()" |
                (i, ws) <- zip [1..] l ]

xmobarWorkspaces = clickable njordWorkspaces
    where clickable l =
            [ "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>" |
                (i, ws) <- zip [1..] l ]

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
