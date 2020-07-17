--     _   __  _                __ 
--    / | / / (_)___  _________/ / 
--   /  |/ / / / __ \/ ___/ __  /  
--  / /|  / / / /_/ / /  / /_/ /   
-- /_/ |_/_/ /\____/_/   \__,_/    
--      /___/                      
--       
-- Author: Lucas Cruz dos Reis(L.C.R.) 
-- Github: https://github.com/LCRERGO 

module XMonad.Njord.Applications
    ( Applications (..)
    , njordApplications )
    where

data Applications = Applications
    { terminal    :: !String
    , browser     :: !String
    , fileBrowser :: !String
    , musicPlayer :: !String
    , calendar    :: !String
    , calculator  :: !String
    , ircClient   :: !String
    }

njordApplications = Applications
    { terminal = "alacritty"
    , browser = "firefox"
    , fileBrowser = "ranger"
    , musicPlayer = "ncmpcpp"
    , calendar = "calcurse"
    , calculator = "ghci"
    , ircClient = "irssi --config=$XDG_CONFIG_HOME/irssi/config --home=$XDG_DATA_HOME/irssi"
    }
