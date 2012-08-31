{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module SnapApp.Controllers where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.ByteString (ByteString)  
import qualified Data.IxSet as IxSet

import           SnapApp.Models
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                 HasAcid (getAcidStore), makeAcidic, update, query, acidInit)

------------------------------------------------------------------------------
-- Seperate our Controller Functions. This way it is explicit that our models
-- are not based on Acid State
------------------------------------------------------------------------------

-- | Look up a user
lookupOpenIdUser :: ByteString -> Query ApplicationState (Maybe OpenIdUser)
lookupOpenIdUser ui = do
	ApplicationState {..} <- ask
	return $ IxSet.getOne $ allUsers IxSet.@= ui

-- | Insert a user (Note, this does not check for unique ID)
insertNewOpenIdUser :: ByteString -> Update ApplicationState OpenIdUser
insertNewOpenIdUser ident = do
	a@ApplicationState{..} <- get
	let newUser = OpenIdUser { openIdIdentifier = ident
							 , name = "" }
	put $ a { allUsers = IxSet.insert newUser allUsers }
	return newUser

makeAcidic ''ApplicationState ['lookupOpenIdUser, 'insertNewOpenIdUser]