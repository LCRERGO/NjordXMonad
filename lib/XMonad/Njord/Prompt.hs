--     _   __  _                __ 
--    / | / / (_)___  _________/ / 
--   /  |/ / / / __ \/ ___/ __  /  
--  / /|  / / / /_/ / /  / /_/ /   
-- /_/ |_/_/ /\____/_/   \__,_/    
--      /___/                      
--       
-- Author: Lucas Cruz dos Reis(L.C.R.) 
-- Github: https://github.com/LCRERGO 

module XMonad.Njord.Prompt
    ( runProgPrompt
    , getPassPrompt
    , connPrompt
    , powPrompt)
    where

-- XMonad Base
import XMonad

-- XMonad Prompts
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Pass (passPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh (sshPrompt)

-- Njord Configuration
import qualified XMonad.Njord.Applications as N
import qualified XMonad.Njord.Colors as N

-- Additional Haskell imports
import System.Exit (exitSuccess)

njordPrompt :: XPConfig
njordPrompt = def
    { font                = "xft:Hack Nerd Font:pixelsize=12"
    , bgColor             = N.grayBlue0
    , fgColor             = N.white0
    , bgHLight            = N.purple0
    , fgHLight            = N.white0
    , borderColor         = "#373b41"
    , promptBorderWidth   = 1
    , position            = Top
    , alwaysHighlight     = True
    , height              = 22
    , maxComplRows        = Just 4
    , historySize         = 256
    , historyFilter       = id
    , promptKeymap        = emacsLikeXPKeymap
    , defaultText         = []
    , autoComplete        = Just 100000
    , showCompletionOnTab = True
    , searchPredicate     = fuzzyMatch
    }

--------------------------------------------------------------------------------
-- Custom prompts
--------------------------------------------------------------------------------
powerManagement :: String -> X ()
powerManagement arg | arg == "poweroff" = spawn ("systemctl " ++ arg)
                    | arg == "reboot" = spawn ("systemctl " ++ arg)
                    | arg == "suspend" = spawn ("systemctl " ++ arg)
                    | arg == "quit" = io exitSuccess

powerPrompt :: XPConfig -> X ()
powerPrompt conf = inputPromptWithCompl conf "Power" powerCompl ?+ powerManagement
  where
    powerCompl = mkComplFunFromList
                 [ "poweroff"
                 , "rebbot"
                 , "suspend"
                 , "quit"
                 ]

--------------------------------------------------------------------------------
-- Default prompts
--------------------------------------------------------------------------------
runProgPrompt = shellPrompt njordPrompt
getPassPrompt = passPrompt njordPrompt
connPrompt = sshPrompt njordPrompt {autoComplete = Nothing}
powPrompt = powerPrompt njordPrompt {autoComplete = Nothing}
