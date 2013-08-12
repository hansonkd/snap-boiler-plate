{-# LANGUAGE OverloadedStrings          #-}

module SnapApp.Forms where

import           Data.ByteString (ByteString)
import           Control.Applicative ((<$>), (<*>))
import           Crypto.PasswordStore (makePassword)
import           Data.Maybe (isJust)
import           Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

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
newPassForm :: OpenIdUser -> Form T.Text AppHandler T.Text
newPassForm user = validateM checkPassword $ (,) <$> "originalPass"      .: checkM "Wrong Password" rightPassword (text Nothing)
                                                 <*> "passphrase"        .: passwordConfirmer
                     where rightPassword pw = confirmOIDPassphrase user pw
                           -- Need to actually make the validation
                           checkPassword (pw, npw) = do
                                         newPass <- liftIO $ makePassword (encodeUtf8 npw) 12
                                         return  $ Success $ decodeUtf8 newPass

-- | See if the passwords are equal
passwordConfirmer :: Monad m => Form T.Text m T.Text
passwordConfirmer = validate checkPasswords $ (,)   <$> ("p1" .: text Nothing)
                                                    <*> ("p2" .: text Nothing)
                    where checkPasswords (p1, p2) | p1 == p2  = Success p1
                                                  | otherwise = Error "Passwords must match"
                                                  
-- | Get a name
newNameForm :: Form T.Text AppHandler T.Text
newNameForm = "name" .: checkM "Name is taken" duplicateName (text Nothing)
        where duplicateName n = (checkDuplicateUserName $ n) >>= (\x -> return $ not x)