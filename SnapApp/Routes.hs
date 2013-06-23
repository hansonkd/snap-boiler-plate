{-# LANGUAGE OverloadedStrings          #-}

module SnapApp.Routes where

import           Data.ByteString (ByteString)

import           Snap.Util.FileServe (serveDirectory)

import           SnapApp.Application (AppHandler)
import           SnapApp.Views

------------------------------------------------------------------------------
-- Just our routes seperate to keep things tidy
------------------------------------------------------------------------------

routeList :: [(ByteString, AppHandler ())]
routeList  = [ ("", serveDirectory "resources/static")
             , ("/", renderMaster)
             , ("/restful/submit",  restfulSubmit)
             , ("/authenticate", authenticate)
             , ("/authenticate/landing", authenticateLanding)
             , ("/authenticate/changePassword/", changePassword)
             , ("/authenticate/changeName/", changeName)
             ]