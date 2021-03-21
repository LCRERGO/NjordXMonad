--     _   __  _                __ 
--    / | / / (_)___  _________/ / 
--   /  |/ / / / __ \/ ___/ __  /  
--  / /|  / / / /_/ / /  / /_/ /   
-- /_/ |_/_/ /\____/_/   \__,_/    
--      /___/                      
--       
-- Author: Lucas Cruz dos Reis(L.C.R.) 
-- Github: https://github.com/LCRERGO 

module XMonad.Njord.Keybindings
    ( njordKeys )
    where

-- XMonad Base
import XMonad
import qualified XMonad.StackSet as W

-- XMonad Actions
import XMonad.Actions.CopyWindow (copy, kill1, killAllOtherCopies)

-- XMonad Layout Modifiers
import XMonad.Layout.LayoutCombinators (JumpToLayout(..))

-- XMonad Utilities
import XMonad.Util.EZConfig
import XMonad.Util.Run (runInTerm)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Njord Configuration
import qualified XMonad.Njord.Applications as N
import qualified XMonad.Njord.Layouts as N
import qualified XMonad.Njord.Misc as N
import qualified XMonad.Njord.Prompt as N

-- Additional Haskell imports
import qualified Data.Map as M
import Data.Ratio ((%))
import System.Exit (exitSuccess)

--------------------------------------------------------------------------------
-- Keybindings
--------------------------------------------------------------------------------
njordKeys :: [(String, X ())]
njordKeys =
    -- Workspace Manipulation
    [ ("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip N.polybarWorkspaces (show <$> [1..])
        , (f, m) <- [(W.view, ""), (W.shift, "S-"), (copy, "C-")]]
    ++
    -- Prompt Selection
    [ ("M-p d", N.runProgPrompt) 
    , ("M-p p", N.getPassPrompt)
    , ("M-p c", N.connPrompt)
    , ("M-p x", N.powPrompt)
    ]
    ++
    -- General Keys
    [ ("M-<Return>"     , spawn $ N.terminal N.njordApplications)
    , ("M-d"            , N.runProgPrompt)
    , ("M-S-c"          , kill1)
    , ("M-<Delete>"     , killAllOtherCopies >> kill1)

    , ("M-n"        , refresh)

    -- move focus up or down the window stack
    , ("M-<Tab>"    , windows W.focusDown)
    , ("M-S-<Tab>"  , windows W.focusUp)
    , ("M-j"        , windows W.focusDown) 
    , ("M-k"        , windows W.focusUp) 
    , ("M-o"        , windows W.focusMaster)

    -- modifying the window order
    , ("M-S-<Return>"   , invertMaster)
    , ("M-S-j"          , windows W.swapDown)
    , ("M-S-k"          , windows W.swapUp)

    -- resizing the master/slave ratio
    , ("M-h"            , sendMessage Shrink)
    , ("M-l"            , sendMessage Expand) 

    -- Floating windows (Set floatable or not)
    , ("M-<Space>"  , withFocused $ windows . ascend)
    , ("M-S-<Space>", withFocused $ windows . W.sink)

    -- Layout Keys
    , ("M-;", nextLayout)
    , ("M-,", prevLayout)
    , ("M-t", sendMessage . JumpToLayout $ "Tiled")
    , ("M-m", sendMessage . JumpToLayout $ "Monitor")
    , ("M-g", sendMessage . JumpToLayout $ "Grid")
    , ("M-s", sendMessage . JumpToLayout $ "Fibbonacci")
    , ("M-c", sendMessage . JumpToLayout $ "FCenterMaster")

    -- Increase/Decrease the number of windows in the master area
    , ("M-i", sendMessage (IncMasterN 1))
    , ("M-u", sendMessage (IncMasterN (-1)))

    -- Window Navigation


    -- Application Keys
    , ("M-C-b", spawn $ N.browser N.njordApplications)
    , ("M-C-f", runInTerm "" (N.fileBrowser N.njordApplications))
    , ("M-C-m", runInTerm "" (N.musicPlayer N.njordApplications))
    , ("M-C-p", runInTerm "" (N.calendar N.njordApplications))
    , ("M-C-c", runInTerm "" (N.calculator N.njordApplications))
    , ("M-C-s", runInTerm "" (N.ircClient N.njordApplications))

    -- quit, or restart
    , ("M-S-q"  , io exitSuccess)
    , ("M-q"    , spawn restartXMonad)

    , ("<Print>", spawn printCmd)
    -- Multimedia keys
    -- Audio Control
    , ("<XF86AudioRaiseVolume>"     , spawn $ volCtl "inc")
    , ("<XF86AudioLowerVolume>"     , spawn $ volCtl "dec")
    , ("<XF86AudioMute>"            , spawn $ volCtl "mute")
    , ("S-<XF86AudioRaiseVolume>"   , spawn $ volCtl "mic inc")
    , ("S-<XF86AudioLowerVolume>"   , spawn $ volCtl "mic dec")
    , ("S-<XF86AudioMute>"          , spawn $ volCtl "mic mute")
    -- Music xPlayer Control
    , ("<XF86AudioPlay>"            , spawn $ playerCtl "toggle")
    , ("S-<XF86AudioPlay>"          , spawn $ playerCtl "stop")
    , ("<XF86AudioPrev>"            , spawn $ playerCtl "prev")
    , ("<XF86AudioNext>"            , spawn $ playerCtl "next")
    -- Screen Brightness
    , ("<XF86MonBrightnessUp>"      , spawn $ brightCtl "inc")
    , ("<XF86MonBrightnessDown>"    , spawn $ brightCtl "dec")
    ]
--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

printCmd :: String
printCmd = "import -window root /tmp/pic-$(date '+%y%m%d-%H%M-%S').jpg"

restartXMonad :: String
restartXMonad = "if type xmonad;" ++
        "then xmonad --recompile && xmonad --restart;" ++
        "else xmessage xmonad not in \\$PATH: \"$PATH\"; fi &&" ++
        "notify-send \"XMonad recompiled with success!\""

ascend :: Window -> W.StackSet i l Window s sd -> W.StackSet i l Window s sd
ascend = flip W.float (W.RationalRect (1%4) (1%4) (1%2) (1%2))

--------------------------------------------------------------------------------
-- Multimedia Functions
-- These are unsafe functions, should not be used out of a controlled 
-- environment.
--------------------------------------------------------------------------------
volCtl :: String -> String
volCtl arg | arg == "inc"      = "amixer -q set Master 5%+"
           | arg == "dec"      = "amixer -q set Master 5%-"
           | arg == "mute"     = "amixer -q set Master toggle"
           | arg == "mic inc"  = "amixer -q set Capture 5%+"
           | arg == "mic dec"  = "amixer -q set Capture 5%-"
           | arg == "mic mute" = "amixer -q set Capture toggle"
playerCtl :: String -> String
playerCtl arg | arg == "toggle" = "mpc toggle"
              | arg == "stop"   = "mpc stop"
              | arg == "prev"   = "mpc prev"
              | arg == "next"   = "mpc next"
brightCtl :: String -> String
brightCtl arg | arg == "inc" = "brightnessctl set +10%"
              | arg == "dec" = "brightnessctl set 10%-"

--------------------------------------------------------------------------------
-- This function inverts the master window with the top window.
-- Is a copy of dwmpromote implementation.
--------------------------------------------------------------------------------
invertMaster :: X ()
invertMaster = windows $ W.modify' $
    \c -> case c of
            W.Stack _ [] []     -> c
            W.Stack t [] (x:rs) -> W.Stack x [] (t:rs)
            W.Stack t ls rs     -> W.Stack t [] (ys ++ x : rs)
                where (x:ys) = reverse ls

--------------------------------------------------------------------------------
-- These functions are simply wrappers for rapid layout management.
--------------------------------------------------------------------------------
nextLayout :: X ()
nextLayout = sendMessage NextLayout

prevLayout :: X ()
prevLayout = do
    winset <- gets windowset
    let ld = description . W.layout . W.workspace . W.current $ winset
    let index = mod (findElem ld N.layoutNames - 1) (length N.layoutNames)
    let pl = N.layoutNames !! index
    sendMessage . JumpToLayout $ pl
        where
            findElem x ls = go x ls 0
            go x (y:ys) n = if x == y then n else go x ys n+1
