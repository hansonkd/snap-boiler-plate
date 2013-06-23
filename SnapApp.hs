{-# LANGUAGE OverloadedStrings          #-}
module SnapApp (app) where
    
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           Snap.Core

import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.AcidState (acidInit)
import           Snap.Snaplet.Heist (heistInit)

import           SnapApp.Application (App (App), acid, sess, heist)
import           SnapApp.Models (initApplicationState)
import           SnapApp.Routes

------------------------------------------------------------------------------
-- Init Code
------------------------------------------------------------------------------

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
	a <- nestSnaplet "acid" acid $ acidInit initApplicationState
	s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing -- | (Just 3600)
	h <- nestSnaplet "" heist $ heistInit "templates"
	addRoutes $ routeList
	return $ App a s h