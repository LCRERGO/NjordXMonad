module XMonad.Util.DBusHook
    ( getDbusHook )
    where

import qualified DBus as D
import qualified DBus.Client as D

getDbusHook :: IO D.Client
getDbusHook = do
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [ D.nameAllowReplacement
        , D.nameReplaceExisting
        , D.nameDoNotQueue
        ]
    return dbus
