{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}


module Main where

import           Snap (SnapletInit, addRoutes, nestSnaplet, serveSnaplet,
                 defaultConfig, makeSnaplet)

import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.AcidState (acidInit)

import           SnapApp.Application (App (App), acid, sess)
import           SnapApp.Models (initApplicationState)
import           SnapApp.Routes (routes)

------------------------------------------------------------------------------
-- Init Code
------------------------------------------------------------------------------

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    a <- nestSnaplet "acid" acid $ acidInit initApplicationState
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing -- | (Just 3600)
    addRoutes routes
    return $ App a s


-- | Use the built in server
main :: IO ()
main = serveSnaplet defaultConfig app