--     _   __  _                __ 
--    / | / / (_)___  _________/ / 
--   /  |/ / / / __ \/ ___/ __  /  
--  / /|  / / / /_/ / /  / /_/ /   
-- /_/ |_/_/ /\____/_/   \__,_/    
--      /___/                      
--       
-- Author: Lucas Cruz dos Reis(L.C.R.) 
-- Github: https://github.com/LCRERGO 

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module XMonad.Njord.Layouts
    ( njordLayoutHook
    , layoutNames )
    where

-- XMonad Base
import XMonad hiding ((|||))

-- XMonad Hooks
import XMonad.Hooks.ManageDocks (avoidStruts)

-- XMonad Layouts
import XMonad.Layout.Grid (defaultRatio, Grid(GridRatio))
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.SimplestFloat (simplestFloat)

-- XMonad Layout Modifiers
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

njordLayoutHook = layoutModifiers layoutsCombined
          where
              layoutModifiers = windowArrange . smartBorders . avoidStruts .
                  windowNavigation
              layoutsCombined =
                  tiled                  |||
                  bDeck                  |||
                  monitor                ||| 
                  grid                   |||
                  fibbonacci             ||| 
                  floatingCenteredMaster ||| 
                  floatLayout

tiled = renamed [Replace "Tiled"] $ spacingDef tall
bDeck = renamed [Replace "BDeck"] $ spacingDef (Mirror tall)
monitor = renamed [Replace "Monitor"] Full
grid = renamed [Replace "Grid"] $ spacingDef gridDef
fibbonacci = renamed [Replace "Fibbonacci"] $ spacingDef (spiral (125 / 146))
floatingCenteredMaster = renamed [Replace "FCenterMaster"] (centerMaster gridDef)
floatLayout = renamed [Replace "Floats"] simplestFloat

-- auxiliary layout functions
gridDef = Mirror $ GridRatio (1/defaultRatio)
spacingDef = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True
tall = Tall 1 (3/100) 0.55

layoutNames :: [String]
layoutNames = [ "Tiled"
              , "BDeck"
              , "Monitor"
              , "Grid"
              , "Fibbonacci"
              , "FCenterMaster"
              , "Floats"
              ]
