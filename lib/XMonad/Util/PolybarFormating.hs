module XMonad.Util.PolybarFormating
    ( polybarColor
    , polybarFont
    , polybarOverline
    , polybarUnderline
    )
    where

-- XMonad Hooks
import XMonad.Hooks.DynamicLog

polybarColor :: String -> String -> String -> String
polybarColor fg bg 
  | null fg = wrap ("%{B" ++ bg ++ "} ") "%{B-}"
  | null bg = wrap ("%{F" ++ fg ++ "} ") "%{F-}"
  | not (null fg || null bg) = wrap ("%{F" ++ fg ++ "}" ++ 
    "%{B" ++ bg ++ "} ") "%{F- B-}"
  | otherwise = id

polybarFont :: String -> String -> String
polybarFont fn = wrap ("%{T" ++ fn ++ "}") "%{T-}"

polybarOverline :: String -> String -> String
polybarOverline oc = wrap ("%{o" ++ oc ++ "}%{+0}") "%{o-}"

polybarUnderline :: String -> String -> String
polybarUnderline uc = wrap ("%{u" ++ uc ++ "}%{+u}") "%{u-}"
