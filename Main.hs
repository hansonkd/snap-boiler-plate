{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception (SomeException, try)
import qualified Data.Text as T
import           System.IO

import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           Snap.Core

import           SnapApp (app)

#ifdef PRODUCTION
import           Snap.Loader.Static
#else
import           Snap.Loader.Dynamic
#endif





main :: IO ()
main = do
    -- Depending on the version of loadSnapTH in scope, this either enables
    -- dynamic reloading, or compiles it without. The last argument to
    -- loadSnapTH is a list of additional directories to watch for changes to
    -- trigger reloads in development mode. It doesn't need to include source
    -- directories, those are picked up automatically by the splice.
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["snaplets/heist/templates"])

    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup

------------------------------------------------------------------------------
-- | This action loads the config used by this application. The loaded config
-- is returned as the first element of the tuple produced by the loadSnapTH
-- Splice. The type is not solidly fixed, though it must be an IO action that
-- produces the same type as 'getActions' takes. It also must be an instance of
-- Typeable. If the type of this is changed, a full recompile will be needed to
-- pick up the change, even in development mode.
--
-- This action is only run once, regardless of whether development or
-- production mode is in use.
getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig


------------------------------------------------------------------------------
-- | This function generates the the site handler and cleanup action from the
-- configuration. In production mode, this action is only run once. In
-- development mode, this action is run whenever the application is reloaded.
--
-- Development mode also makes sure that the cleanup actions are run
-- appropriately before shutdown. The cleanup action returned from loadSnapTH
-- should still be used after the server has stopped handling requests, as the
-- cleanup actions are only automatically run when a reload is triggered.
--
-- This sample doesn't actually use the config passed in, but more
-- sophisticated code might.
getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet
        (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)