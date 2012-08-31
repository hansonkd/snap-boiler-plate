{-# LANGUAGE OverloadedStrings #-}

module SnapApp.Templates.ExampleBlazeTemplate where

import Control.Monad (forM_)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


-- | A Basic Blaze Template that will generate a list of numbers
numbers :: Int -> H.Html
numbers n = H.docTypeHtml $ do
     H.head $ do
         H.title "Natural numbers"
     H.body $ do
         H.p "A list of natural numbers:"
         H.ul $ forM_ [1 .. n] (H.li . H.toHtml)