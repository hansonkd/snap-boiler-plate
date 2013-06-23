{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module SnapApp.Application where

import           Control.Lens

import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session        
import           Snap.Snaplet.AcidState
          
import           SnapApp.Models

------------------------------------------------------------------------------
-- App DataType
------------------------------------------------------------------------------
                     
data App = App
    { _acid        :: Snaplet (Acid ApplicationState)
	, _sess        :: Snaplet SessionManager
	, _heist       :: Snaplet (Heist App)
    }

type AppHandler = Handler App App

makeLenses ''App

-- | So we don't have to use with acid
instance HasAcid App ApplicationState where
    getAcidStore = view (acid.snapletValue)

instance HasHeist App where
    heistLens = subSnaplet heist