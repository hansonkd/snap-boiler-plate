{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module SnapApp.Application where

import           Prelude hiding ((.))
import           Control.Category ((.))

import           Snap (SnapletInit, Snaplet, Handler, 
                 addRoutes, nestSnaplet, serveSnaplet,
                 defaultConfig, makeSnaplet, 
                 snapletValue, writeText, 
                 makeLens, getL, modL, modify, method)
                 
import           Snap.Snaplet.AcidState (Acid, HasAcid (getAcidStore))
import           Snap.Snaplet.Session
          
import           SnapApp.Models


------------------------------------------------------------------------------
-- App DataType
------------------------------------------------------------------------------
                     
data App = App
    { _acid :: Snaplet (Acid ApplicationState)
	, _sess :: Snaplet SessionManager
    }

type AppHandler = Handler App App

makeLens ''App

-- | So we don't have to use with acid
instance HasAcid App ApplicationState where
    getAcidStore = getL (snapletValue . acid)