{-# LANGUAGE OverloadedStrings #-}
module SnapApp.Utils where
        
import qualified Data.Text as T
import           Data.String (fromString)

import           Data.UUID.V4
import qualified Data.UUID (toString)

------------------------------------------------------------------------------
-- General Stuff
------------------------------------------------------------------------------

-- | Generate a V4 UUID
getUUID :: IO T.Text
getUUID = nextRandom >>= \ui -> return $ T.pack $ Data.UUID.toString ui