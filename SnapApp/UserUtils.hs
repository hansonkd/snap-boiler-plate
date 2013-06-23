{-# LANGUAGE OverloadedStrings          #-}

module SnapApp.UserUtils where
    
import           Control.Monad (when, void)
import           Crypto.PasswordStore
import           Data.Maybe (fromMaybe, fromJust, isNothing, isJust)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T

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
restrictAccess_ = void restrictAccess
    
-- | Restrict Access but return the logged in user
-- | Redirects to Signin Page
restrictAccess :: AppHandler (OpenIdUser)
restrictAccess = do
    cu <- getCurrentUser
    when (isNothing cu) $ redirect (encodeUtf8 "/authenticate")
    return $ fromJust cu

-- | Just see if the phrase is the one on file. 
-- | Since this field is empty, if it is empty just return true
confirmOIDPassphrase :: OpenIdUser -> T.Text -> AppHandler (Bool)
confirmOIDPassphrase user pw = case uploadPassPhrase user of
                             ""   -> return True
                             pwh  -> return $ verifyPassword (encodeUtf8 pw) (encodeUtf8 pwh)
                                     
-- | Check to see if we have a duplicate username on file
checkDuplicateUserName :: T.Text -> AppHandler (Bool)
checkDuplicateUserName un = do
    possibleUser <- query $ LookupName un
    return $ isJust possibleUser

-- | Create a new user with the OpenID
-- | Set name to default value and passphrase to nothing
makeNewUser :: T.Text -> AppHandler OpenIdUser
makeNewUser openId = do
    uuid <- liftIO getUUID
    update (InsertNewOpenIdUser uuid openId "Lurker" "")

-- | get a OpenIDUser By The Cookie
getCurrentUser :: AppHandler (Maybe OpenIdUser)
getCurrentUser = do
    withTop sess $ do
        sessRes <- getFromSession "__user_id"
        return =<< query $ LookupOpenIdUser $ fromMaybe "" sessRes

-- | Create a user or get a user
loginOrCreate :: T.Text -> AppHandler (OpenIdUser, Bool)
loginOrCreate ui = do
	possibleUser <- query $ LookupOpenIdUser ui
	case possibleUser of
			Just user -> return (user, False)
			Nothing   -> makeNewUser ui >>= \u -> return (u, True)

-- | Set a session and login the user
checkin :: T.Text -> AppHandler (OpenIdUser, Bool)
checkin ui = do
	(user, created) <- loginOrCreate ui
	withSession sess $ withTop sess $ setInSession "__user_id" (openIdIdentifier user)
	return (user, created)