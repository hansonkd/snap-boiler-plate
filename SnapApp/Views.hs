{-# LANGUAGE OverloadedStrings          #-}

module SnapApp.Views where
    
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.ByteString.UTF8 (fromString)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.HTTP.Conduit (withManager)
import qualified Web.Authenticate.OpenId as OpenId
import           Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL

import           SnapApp.Controllers
import           SnapApp.Models
import           SnapApp.Forms
import           SnapApp.Application (AppHandler, sess)
import           SnapApp.UserUtils

import           Snap.Snaplet.Session
import           Snap.Snaplet (withTop)
import           Snap.Core

import           Snap.Snaplet.AcidState (update, query)
import           Snap.Snaplet.Heist (heistLocal, render)
import           Text.Digestive.Snap hiding (method)
import           Text.Digestive.Heist (bindDigestiveSplices)

------------------------------------------------------------------------------
-- Our Views
------------------------------------------------------------------------------

-- | Render our master template
renderMaster :: AppHandler ()
renderMaster = render "home"

-- | RESTFul API for CLI Utility
restfulSubmit :: AppHandler ()
restfulSubmit = do
        modifyResponse (setContentType "application/json")
        method POST process <|> errorMessage
    where
        process = do
            req <- getRequest
            let inp = rqPostParams req
            writeBS (fromString $ show inp)
        errorMessage = do
            writeBS ("POST Requests only")
        
-- | Change your name (this can only be done once from default)
changeName :: AppHandler ()
changeName = do
    user <- restrictAccess
    case name user of
        "Lurker" -> do
            (view, result) <- runForm "form" $ newNameForm
            case result of
                Just x -> do
                    update (ChangeUser (user { name = x }))
                    writeBS (encodeUtf8 x)
                Nothing -> do
                    heistLocal (bindDigestiveSplices view) $ render "newName"
        _        -> writeBS ("Sorry you can't change your name")
        
-- | A small view that processes a Digestive Functor Form (Doesn't do anything
-- | Functional)
changePassword :: AppHandler ()
changePassword = do
    user <- restrictAccess
    (view, result) <- runForm "form" $ newPassForm user
    case result of
            Just x  -> do
                    update (ChangeUser (user { uploadPassPhrase = x }))
                    writeBS (encodeUtf8 x)
            Nothing -> do
            	heistLocal (bindDigestiveSplices view) $ render "newPassword"

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
					(user, created) <- checkin (OpenId.identifier $ ident)
					case created of
						False -> writeBS ("Welcome Back")
						True  -> writeBS ("Hello New User")
			Nothing    -> writeBS ("Unable to Login")