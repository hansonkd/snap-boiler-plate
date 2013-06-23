{-# LANGUAGE OverloadedStrings #-}
module SnapApp.Utils where
        
import           Data.ByteString (ByteString)
import           Data.String (fromString)

import           Data.UUID.V4
import qualified Data.UUID (toString)

------------------------------------------------------------------------------
-- General Stuff
------------------------------------------------------------------------------

-- | Generate a V4 UUID
getUUID :: IO ByteString
getUUID = nextRandom >>= \ui -> return $ fromString $ Data.UUID.toString ui