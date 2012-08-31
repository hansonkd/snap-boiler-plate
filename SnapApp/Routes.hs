{-# LANGUAGE OverloadedStrings          #-}

module SnapApp.Routes (routes) where

import           Data.ByteString (ByteString)


import           Snap.Util.FileServe (serveDirectory)

import           SnapApp.Application (AppHandler)
import           SnapApp.Views

------------------------------------------------------------------------------
-- Just our routes seperate to keep things tidy
------------------------------------------------------------------------------

routes :: [(ByteString, AppHandler ())]
routes = [ ("", serveDirectory "resources/static")
         , ("/numbers", renderNumbers)
		 , ("/userId", userIdSession)
		 , ("/blazeTest", digestiveTest)
		 , ("/authenticate", authenticate)
		 , ("/authenticate/landing", authenticateLanding)
         ]