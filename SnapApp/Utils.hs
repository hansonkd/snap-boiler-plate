{-# LANGUAGE OverloadedStrings #-}
module SnapApp.Utils
    ( renderBlaze
	, renderBlazeForm
    ) where

import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Snap.Core (MonadSnap (..), addHeader, modifyResponse, writeLBS)
import qualified Data.Text as T

import Text.Digestive
import Text.Digestive.Blaze.Html5 as DB
import Text.Digestive.Util

------------------------------------------------------------------------------
-- Blaze Helper Code
------------------------------------------------------------------------------

-- | Render a Blaze Template in Snap
renderBlaze :: MonadSnap m => Html -> m ()
renderBlaze response = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml response

-- | Wrap a DF form in a <form> tag and add a submit button
renderBlazeForm :: View T.Text -> (View Html -> Html) -> Html
renderBlazeForm view fview = do
	let view' = fmap H.toHtml view
	DB.form view' "" $ do
		fview view'
		H.br
		DB.inputSubmit "Submit"