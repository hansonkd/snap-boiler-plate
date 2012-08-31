{-# LANGUAGE OverloadedStrings          #-}

module SnapApp.Views where
    
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.Map as M
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.HTTP.Conduit (withManager)
import qualified Web.Authenticate.OpenId as OpenId

import           SnapApp.Controllers
import           SnapApp.Models
import           SnapApp.Templates.ExampleBlazeTemplate (numbers)
import           SnapApp.Forms (userForm, userView)
import           SnapApp.Utils (renderBlaze, renderBlazeForm)
import           SnapApp.Application (AppHandler, sess)

import           Snap.Snaplet.Session
import           Snap.Snaplet (withTop)
import           Snap (Handler)
import           Snap.Core

import           Snap.Snaplet.AcidState (update, query)
import           Text.Digestive.Snap hiding (method)

------------------------------------------------------------------------------
-- Our Views
------------------------------------------------------------------------------

-- | Render our example Template
renderNumbers :: AppHandler ()
renderNumbers = do
    renderBlaze $ numbers 10

-- | A small view that processes a Digestive Functor Form (Doesn't do anything
-- | Functional)
digestiveTest :: AppHandler ()
digestiveTest = do
	(view, result) <- runForm "form" userForm
	case result of
			Just x  -> writeBS("Recieved")
			Nothing -> do
				renderBlaze $ renderBlazeForm view userView

-- | Displays a form in to input a openId. Shows how to get raw POST data w/o
-- | Digestive Functors
authenticate :: AppHandler ()
authenticate = do
        modifyResponse (setContentType "text/html")
        method POST process <|> form
    where
        form = do
            writeBS "Display form...<form method='POST'><input name='openid'></form>"
        process = do
			inp <- getParam "openid"
			case inp of
				Just x -> login x
				Nothing -> writeBS ("error")
			writeBS (fromString $ show inp)
            
-- | The OpenID Function
login x = do
	url <- liftIO $ withManager $ OpenId.getForwardUrl 
		(decodeUtf8 x) "http://localhost:8000/authenticate/landing" Nothing []
	redirect (encodeUtf8 url)

-- | Demonstrates how to use Snap Sessions to retrieve our OpenId
userIdSession :: AppHandler ()
userIdSession = do
	modifyResponse (setContentType "text/html")
	withTop sess $ do
		mui <- getFromSession "__user_id"
		writeBS (fromString $ show mui)

-- | Create a user or get a user
loginOrCreate :: ByteString -> AppHandler (OpenIdUser, Bool)
loginOrCreate ui = do
	possibleUser <- query $ LookupOpenIdUser ui
	case possibleUser of
			Just user -> return (user, False)
			Nothing   -> update (InsertNewOpenIdUser ui) >>= \u -> return (u, True)

-- | Set a session and login the user
checkin :: ByteString -> AppHandler (OpenIdUser, Bool)
checkin ui = do
	(user, created) <- loginOrCreate ui
	withSession sess $ withTop sess $ setInSession "__user_id" (fromString $ show $ openIdIdentifier user)
	return (user, created)

-- | Helper function to Convert Snap Params to OpenId Param Format
convertParams :: Params -> [(T.Text, T.Text)]
convertParams params = [(decodeUtf8 k, decodeUtf8 (head v))| (k, v) <- (M.toList params)]

-- | This is our view that handles when a user logs in and the OpenId redirects back to us
-- | so that we can complete the login
authenticateLanding :: AppHandler ()
authenticateLanding = do
	req <- getRequest
	oir <- liftIO $ withManager $ OpenId.authenticateClaimed (convertParams (rqParams req)) :: AppHandler (OpenId.OpenIdResponse)
	case OpenId.oirClaimed oir of
			Just ident -> do
					(user, created) <- checkin (encodeUtf8 $ OpenId.identifier $ ident)
					case created of
						False -> writeBS ("Welcome Back")
						True  -> writeBS ("Hello New User")
			Nothing    -> writeBS ("Unable to Login")