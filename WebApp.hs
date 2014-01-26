{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts,
             FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
                
import Data.Monoid       
import Data.Traversable hiding (mapM)
import Control.Applicative ((<$>))
import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent.STM       

import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import Control.Error

import Network.HTTP.Types.Status (status500, status404)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=))
import qualified Data.Aeson as JSON
import Web.Scotty.Trans hiding (ScottyM, ActionM)

import DataLogger (DataLogger, DeviceId)
import qualified DataLogger as DL

deriving instance Parsable DeviceId

newtype DeviceName = DN String
                   deriving (Show, Eq, Ord, ToJSON, FromJSON, Parsable)

instance ToJSON DeviceId where
    toJSON (DL.DevId devId) = toJSON devId

data Device = Device { devName   :: DeviceName -- ^ Friendly name
                     , devPath   :: FilePath   -- ^ Path to serial device
                     , devLogger :: DataLogger -- ^ @DataLogger@
                     , devId     :: DeviceId   -- ^ Unique @DeviceId@
                     }
                    
type DeviceList = TVar (M.Map DeviceId Device)
type ScottyM = ScottyT TL.Text (ReaderT DeviceList IO)
type ActionM = ActionT TL.Text (ReaderT DeviceList IO)

instance (ScottyError e, Monad m)
       => MonadReader DeviceList (ActionT e (ReaderT DeviceList m)) where
    ask = lift ask
    local f = undefined
    reader f = lift $ reader f

main = do 
    devices <- newTVarIO M.empty
    runReaderT refreshDevices devices
    let run m = runReaderT m devices
    scottyT 3000 run run routes

withDeviceList :: (MonadReader DeviceList m, MonadIO m)
               => (M.Map DeviceId Device -> m a) -> m a
withDeviceList m = ask >>= liftIO . atomically . readTVar >>= m

filterDevicesByName :: DeviceName -> ReaderT DeviceList IO [Device]
filterDevicesByName name =
    withDeviceList $ return . filter (\dev->devName dev == name) . M.elems

lookupDeviceId :: DeviceId -> ReaderT DeviceList IO (Maybe Device)
lookupDeviceId devId =
    withDeviceList $ return . M.lookup devId
    
refreshDevices :: ReaderT DeviceList IO ()
refreshDevices = do
    loggers <- liftIO DL.findDataLoggers
    devList <- ask
    dls <- liftIO $ atomically $ readTVar devList
    let knownDevices = M.elems dls
    
    -- Make sure no devices have disappeared
    forM_ (M.assocs dls) $ \(devId, dev)->do
        when (devPath dev `notElem` loggers)
          $ liftIO $ atomically $ modifyTVar devList $ M.delete devId

    -- Add new devices
    let newDevs = filter (`notElem` map devPath knownDevices) loggers
    forM_ newDevs $ \devPath->runEitherT $ addDevice devPath

addDevice :: MonadIO m
          => FilePath -> EitherT String (ReaderT DeviceList m) DeviceId
addDevice devPath = do
    dl <- DL.open devPath
    devList <- lift ask
    devId <- DL.getDeviceId dl
    name <- DL.get dl DL.deviceName
    liftIO $ atomically $ modifyTVar devList
           $ M.insert devId $ Device { devPath   = devPath
                                     , devLogger = dl
                                     , devId     = devId
                                     , devName   = DN name
                                     }
    return devId

withDevice :: (Device -> ActionM ()) -> ActionM ()
withDevice action = do
    dId <- param "device"
    devices <- lift $ lookupDeviceId dId
    liftIO $ print (dId, fmap devId devices)
    withDeviceList $ liftIO . print . map devId . M.elems
    case devices of
      Nothing  -> do status status404
                     html "Can't find device"
      Just dev -> action dev

getSetting :: (ToJSON a)
           => String -> DL.Setting a -> ScottyM ()
getSetting settingName setting =
    get (capture $ "/devices/:device/"<>settingName) $ withDevice $ \dev->do
        value <- liftIO $ runEitherT $ DL.get (devLogger dev) setting
        case value of
          Left error  -> do
            text $ TL.pack error
            status status500 
          Right value -> do
            json $ toJSON value

putSetting :: (FromJSON a, ToJSON a)
           => String -> DL.Setting a -> ScottyM ()
putSetting settingName setting =
    put (capture $ "/devices/:device/"<>settingName) $ withDevice $ \dev->do
        value <- jsonData
        liftIO $ runEitherT $ DL.set (devLogger dev) setting value
        json $ toJSON value
    
getPutSetting :: (ToJSON a, FromJSON a)
              => String -> DL.Setting a -> ScottyM ()
getPutSetting name setting = do
    getSetting name setting              
    putSetting name setting

instance ToJSON DL.Sample where
    toJSON s =
      JSON.object [ "time"   .= DL.sampleTime s
                  , "sensor" .= case DL.sampleSensor s of DL.SID n -> n
                  , "value"  .= DL.sampleValue s
                  ]

routes :: ScottyM () 
routes = do
    getPutSetting "acquiring" DL.acquiring
    getPutSetting "sample-period" DL.samplePeriod
    getPutSetting "rtc-time" DL.rtcTime
    getPutSetting "acquire-on-boot" DL.acquireOnBoot
    getPutSetting "name" DL.deviceName

    get "/devices" $ do
        withDeviceList $ json . M.keys
    post "/devices" $ do
        lift refreshDevices
        withDeviceList $ json . M.keys
        
    put "/devices/:device/start" $ withDevice $ \dev->do
        liftIO $ runEitherT $ do
            checkRTCTime (devLogger dev)
            DL.set (devLogger dev) DL.acquiring True
        json [ ("device", toJSON (devName dev))
             , ("acquiring" :: String, toJSON True)
             ]

    put "/devices/:device/stop" $ withDevice $ \dev->do
        liftIO $ runEitherT $ DL.set (devLogger dev) DL.acquiring False
        json [ ("device", toJSON (devName dev))
             , ("acquiring" :: String, toJSON False)
             ]

    get "/devices/:device/samples" $ withDevice $ \dev->do
        result <- liftIO $ runEitherT $ DL.getSamples (devLogger dev) 0 100
        case result of 
          Right samples -> json samples
          Left error    -> do html "<h1>Error fetching samples</h1>"
                              status status500

    get "/" $ file "index.html"
    get "/jquery.js" $ file "jquery-2.0.3.js"
    get "/ui.js" $ file "ui.js"
    get "/app.css" $ file "app.css"
    get "/app.js" $ file "app.js"

-- | Check that the RTC time has been set    
checkRTCTime :: DataLogger -> EitherT String IO ()
checkRTCTime dl = do
    t <- DL.get dl DL.rtcTime
    when (t == 0) $ do
      realTime <- liftIO getPOSIXTime
      DL.set dl DL.rtcTime (round $ realToFrac realTime)
