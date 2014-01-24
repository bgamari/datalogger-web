{-# LANGUAGE OverloadedStrings #-}
                
import Web.Scotty                
import Control.Error
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import Data.Monoid       
import Control.Monad (forM_, when)
import Network.HTTP.Types.Status (status500)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=))
import qualified Data.Aeson as JSON
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent.STM       

import DataLogger (DataLogger)
import qualified DataLogger as DL

main = do 
    Right dl <- runEitherT $ DL.open "/dev/ttyACM0"
    scotty 3000 $ routes dl
   
getSetting :: ToJSON a => DataLogger -> String -> DL.Setting a -> ScottyM ()
getSetting dl name setting =
    get (capture $ "/:device/"<>name) $ do
        device <- param "device" :: ActionM String
        value <- liftIO $ runEitherT $ DL.get dl setting
        case value of
          Left error  -> do
            text $ TL.pack error
            status status500 
          Right val   -> do
            json [ ("device" :: String, toJSON device :: JSON.Value)
                 , ("setting", toJSON name)
                 , ("value", toJSON val)
                 ]

putSetting :: FromJSON a => DataLogger -> String -> DL.Setting a -> ScottyM ()
putSetting dl name setting =
    put (capture $ "/:device/"<>name) $ do
        device <- param "device" :: ActionM String
        value <- jsonData
        liftIO $ runEitherT $ DL.set dl setting value
        return ()
    
getPutSetting :: (ToJSON a, FromJSON a)
              => DataLogger -> String -> DL.Setting a -> ScottyM ()
getPutSetting dl name setting = do
    getSetting dl name setting              
    putSetting dl name setting

instance ToJSON DL.Sample where
    toJSON s =
      JSON.object [ "time"   .= DL.sampleTime s
                  , "sensor" .= case DL.sampleSensor s of DL.SID n -> n
                  , "value"  .= DL.sampleValue s
                  ]

routes :: DataLogger -> ScottyM () 
routes dl = do
    getPutSetting dl "acquiring" DL.acquiring
    getPutSetting dl "sample-period" DL.samplePeriod
    getPutSetting dl "rtc-time" DL.rtcTime
    getPutSetting dl "acquire-on-boot" DL.acquireOnBoot

    put "/:device/start" $ do
        device <- param "device"
        liftIO $ runEitherT $ do
            checkRTCTime dl
            DL.set dl DL.acquiring True
        json [("device", device), ("acquiring" :: String, "true" :: TL.Text)]

    put "/:device/stop" $ do
        device <- param "device"
        liftIO $ runEitherT $ DL.set dl DL.acquiring False
        json [("device", device), ("acquiring" :: String, "false" :: TL.Text)]

    get "/:device/samples" $ do
        result <- liftIO $ runEitherT $ DL.getSamples dl 0 100
        case result of 
          Right samples -> json samples
          Left error    -> do html "<h1>Error fetching samples</h1>"
                              status status500

    get "/" $ file "index.html"
    get "/jquery.js" $ file "jquery-2.0.3.js"
    get "/app.css" $ file "app.css"
    get "/app.js" $ file "app.js"

-- | Check that the RTC time has been set    
checkRTCTime :: DataLogger -> EitherT String IO ()
checkRTCTime dl = do
    t <- DL.get dl DL.rtcTime
    when (t == 0) $ do
      realTime <- liftIO getPOSIXTime
      DL.set dl DL.rtcTime (round $ realToFrac realTime)
