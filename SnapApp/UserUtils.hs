{-# LANGUAGE OverloadedStrings          #-}

module SnapApp.UserUtils where
    
import           Control.Monad (when)
import           Crypto.PasswordStore
import           Data.ByteString (ByteString)
import           Data.ByteString.UTF8 (fromString)
import           Data.Maybe (fromMaybe, fromJust, isNothing)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           SnapApp.Application (AppHandler, sess)
import           SnapApp.Controllers
import           SnapApp.Models
import           SnapApp.Utils (getUUID)

import           Snap
import           Snap.Core
import           Snap.Snaplet (withTop)
import           Snap.Snaplet.AcidState (update, query)
import           Snap.Snaplet.Session

-- | Restrict Access but throw away the result
restrictAccess_ :: AppHandler ()
restrictAccess_ = do
    cu <- restrictAccess
    return ()
    
-- | Restrict Access but return the logged in user
-- | Redirects to Signin Page
restrictAccess :: AppHandler (OpenIdUser)
restrictAccess = do
    cu <- getCurrentUser
    when (isNothing cu) $ redirect (encodeUtf8 "/authenticate")
    return $ fromJust cu

-- | Just see if the phrase is the one on file. 
-- | Since this field is empty, if it is empty just return true
confirmOIDPassphrase :: OpenIdUser -> ByteString -> AppHandler (Bool)
confirmOIDPassphrase user pw = case uploadPassPhrase user of
                             ""   -> return True
                             pwh  -> return $ verifyPassword pw pwh
                                     
-- | Check to see if we have a duplicate username on file
checkDuplicateUserName :: ByteString -> AppHandler (Bool)
checkDuplicateUserName un = do
    possibleUser <- query $ LookupName un
    case possibleUser of
        Just _  -> return True
        Nothing -> return False

-- | Create a new user with the OpenID
-- | Set name to default value and passphrase to nothing
makeNewUser :: ByteString -> AppHandler OpenIdUser
makeNewUser openId = do
    uuid <- liftIO getUUID
    update (InsertNewOpenIdUser uuid openId "Lurker" "")

-- | get a OpenIDUser By The Cookie
getCurrentUser :: AppHandler (Maybe OpenIdUser)
getCurrentUser = do
    withTop sess $ do
        sessRes <- getFromSession "__user_id"
        return =<< query $ LookupOpenIdUser $ encodeUtf8 $ fromMaybe "" sessRes

-- | Create a user or get a user
loginOrCreate :: ByteString -> AppHandler (OpenIdUser, Bool)
loginOrCreate ui = do
	possibleUser <- query $ LookupOpenIdUser ui
	case possibleUser of
			Just user -> return (user, False)
			Nothing   -> makeNewUser ui >>= \u -> return (u, True)

-- | Set a session and login the user
checkin :: ByteString -> AppHandler (OpenIdUser, Bool)
checkin ui = do
	(user, created) <- loginOrCreate ui
	withSession sess $ withTop sess $ setInSession "__user_id" (decodeUtf8 $ openIdIdentifier user)
	return (user, created)