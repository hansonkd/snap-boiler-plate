{-# LANGUAGE OverloadedStrings          #-}

module SnapApp.Forms where
    
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H

import Text.Digestive
import Text.Digestive.Blaze.Html5 as DB
import Text.Digestive.Util

------------------------------------------------------------------------------
-- Our Forms using Digestive Functors
------------------------------------------------------------------------------

-- | DataType to hold the data we get from our form
data UserData = UserData
	{ userName :: T.Text
	, userMail :: T.Text
	} deriving (Show)

-- | A user form to show how to use Digestive Functors
userForm :: Monad m => Form T.Text m UserData
userForm = UserData
	<$> "name" .: text Nothing
	<*> "mail" .: check "Not a valid email address" checkEmail (text Nothing)

-- | Basic field check for email
checkEmail :: T.Text -> Bool
checkEmail = isJust . T.find (== '@')

-- | Blaze Template that renders the form	 
userView :: View H.Html -> H.Html
userView view = do
	label     "name" view "Name: "
	inputText "name" view
	H.br
	
	errorList "mail" view
	label     "mail" view "Email address: "
	inputText "mail" view
	H.br
	