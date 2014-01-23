{-# LANGUAGE OverloadedStrings, ExistentialQuantification, RankNTypes #-}
                
import Web.Scotty                
import qualified DataLogger as DL
import Control.Error
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import Data.Monoid       
import Control.Monad (forM_, when)
import Network.HTTP.Types.Status (status500)
import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as JSON
import Data.Time.Clock.POSIX (getPOSIXTime)

main = do 
    Right dl <- runEitherT $ DL.open "/dev/ttyACM0"
    scotty 3000 $ routes dl
   
data JSONSetting = forall a. ToJSON a => JS (DL.Setting a)

loggerSettings :: [(String, JSONSetting)]
loggerSettings = [("acquiring", JS DL.acquiring)]

routes dl = do
    forM_ loggerSettings $ \(name, JS setting)->do
        get (capture $ "/:device/:"<>name) $ do
            device <- param "device" :: ActionM String
            value <- liftIO $ runEitherT $ DL.get dl DL.acquiring
            case value of
              Left error  -> do
                 text $ TL.pack error
                 status status500 
              Right val   -> do
                 json [ ("device" :: String, toJSON device :: JSON.Value)
                      , ("setting", toJSON name)
                      , ("value", toJSON val)
                      ]

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

    get "/hello-world" $ do
        Right version <- liftIO $ runEitherT $ DL.getVersion dl
        html $ "<h1>Hello World</h1>"<>TL.pack version

-- | Check that the RTC time has been set    
checkRTCTime :: DL.DataLogger -> EitherT String IO ()
checkRTCTime dl = do
    t <- DL.get dl DL.rtcTime
    when (t == 0) $ do
      realTime <- liftIO getPOSIXTime
      DL.set dl DL.rtcTime (round $ realToFrac realTime)
