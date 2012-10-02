{-# LANGUAGE OverloadedStrings #-}

module SnapApp.Templates.MasterTemplate where


import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Snap (makeLens, modL)

data BaseTemplate = BaseTemplate { baseTitle :: H.Html
                                 , baseCSS   :: Maybe H.Html
                                 , baseJS    :: Maybe H.Html
                                 , extraJS   :: Maybe H.Html
                                 , baseBody  :: H.Html }

sampleBody :: H.Html
sampleBody = do
    H.h1 "Sample Page"
    H.p  "You should override this."

initTemplate = BaseTemplate "MySite" Nothing Nothing Nothing sampleBody

renderBaseTemplate :: BaseTemplate -> H.Html
renderBaseTemplate bt = H.docTypeHtml $ do
    H.head $ do
        H.title $ baseTitle bt
        H.style $ fromMaybe "" (baseCSS bt)
    H.body $ do
        baseBody bt
        H.script $ fromMaybe "" (baseJS bt)
        H.script $ fromMaybe "" (extraJS bt)