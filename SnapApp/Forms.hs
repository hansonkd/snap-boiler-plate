{-# LANGUAGE OverloadedStrings          #-}

module SnapApp.Forms where

import           Data.ByteString (ByteString)
import           Data.ByteString.UTF8 (toString, fromString)
import           Control.Applicative ((<$>), (<*>))
import           Crypto.PasswordStore (makePassword)
import           Data.Maybe (isJust)
import           Data.Text as T

import           Text.Digestive
import           Text.Digestive.Util

import           Snap (liftIO)
import           SnapApp.Application (AppHandler)
import           SnapApp.Models (OpenIdUser)
import           SnapApp.UserUtils (confirmOIDPassphrase, checkDuplicateUserName)

------------------------------------------------------------------------------
-- Our Forms using Digestive Functors
------------------------------------------------------------------------------

-- | Change Passwords
newPassForm :: OpenIdUser -> Form T.Text AppHandler String
newPassForm user = validateM checkPassword $ (,) <$> "originalPass"      .: checkM "Wrong Password" rightPassword (string Nothing)
                                                 <*> "passphrase"        .: passwordConfirmer
                     where rightPassword pw = confirmOIDPassphrase user (fromString pw)
                           -- Need to actually make the validation
                           checkPassword (pw, npw) = do
                                         newPass <- liftIO $ makePassword (fromString npw) 12
                                         return  $ Success $ toString newPass

-- | See if the passwords are equal
passwordConfirmer :: Monad m => Form T.Text m String
passwordConfirmer = validate checkPasswords $ (,)   <$> ("p1" .: string Nothing)
                                                    <*> ("p2" .: string Nothing)
                    where checkPasswords (p1, p2) | p1 == p2  = Success p1
                                                  | otherwise = Error "Passwords must match"
                                                  
-- | Get a name
newNameForm :: Form T.Text AppHandler String
newNameForm = "name" .: checkM "Name is taken" duplicateName (string Nothing)
        where duplicateName n = (checkDuplicateUserName $ fromString n) >>= (\x -> return $ not x) 
            
{-                                                  
-- | Blaze Template that renders the form	 
newNameView :: View H.Html -> H.Html
newNameView view = do
    H.h2 "New Name"
    errorList "name" view
    inputText "name" view
    H.br                                        
    
-- | Blaze Template that renders the form	 
newPassView :: View H.Html -> H.Html
newPassView view = do
    errorList "originalPass" view
    label     "originalPass" view "Current Password: "
    inputPassword "originalPass" view
    H.br
	
    H.h2 "New Password"
    errorList "passphrase" view
    passValidView $ subView "passphrase" view
    H.br
    
-- | The sub view
passValidView :: View H.Html -> H.Html
passValidView view = do
    label         "p1" view "New Password: "
    inputPassword "p1" view
    H.br
    
    label         "p2" view "Confirm: "
    inputPassword "p2" view
-}
	