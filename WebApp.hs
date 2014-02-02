{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TypeSynonymInstances #-}
                
import Data.Monoid       
import Data.Traversable hiding (mapM)
import Control.Applicative ((<$>))
import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Concurrent.STM       

import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Csv as Csv
import Control.Error

import Network.HTTP.Types.Status (status200, status202, status500, status404)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=))
import qualified Data.Aeson as JSON
import Web.Scotty.Trans hiding (ScottyM, ActionM)

import DataLogger (DataLogger, DeviceId, Sample)
import qualified DataLogger as DL
import DeviceList

deriving instance Parsable DeviceId
deriving instance Parsable DeviceName
deriving instance Parsable DL.SensorId

deriving instance ToJSON DeviceId
deriving instance ToJSON DL.SensorId

instance ToJSON DL.Sensor where
    toJSON s = JSON.object [ "sensor_id" .= DL.sensorId s
                           , "name"      .= DL.sensorName s
                           , "unit"      .= DL.sensorUnit s
                           ]

type ScottyM = ScottyT TL.Text (DeviceListT IO)
type ActionM = ActionT TL.Text (DeviceListT IO)

main = do 
    devList <- newDeviceList
    let run = runDeviceListT devList
    run refreshDevices
    scottyT 3000 run run routes

withDevice :: (Device -> ActionM ()) -> ActionM ()
withDevice action = do
    dId <- param "device"
    devices <- lift $ lookupDeviceId dId
    case devices of
      Nothing  -> do status status404
                     html "Can't find device"
      Just dev -> action dev

withBackedDevice :: (Device -> DataLogger -> ActionM ()) -> ActionM ()
withBackedDevice action = withDevice $ \dev->do
    case devLogger dev of
      Nothing  -> do status status404
                     html "unbacked device"
      Just dl  -> action dev dl
    
getSetting :: (ToJSON a)
           => String -> DL.Setting a -> ScottyM ()
getSetting settingName setting =
    get (capture $ "/devices/:device/"<>settingName) $ withBackedDevice $ \dev dl->do
        value <- liftIO $ runEitherT $ DL.get dl setting
        case value of
          Left error  -> do
            text $ TL.pack error
            status status500 
          Right value -> do
            json $ JSON.object [ "setting"  .= settingName
                               , "deviceId" .= devId dev
                               , "value"    .= toJSON value
                               ]

putSetting :: (Parsable a, ToJSON a)
           => String -> DL.Setting a -> Bool -> ScottyM ()
putSetting settingName setting saveNV =
    post (capture $ "/devices/:device/"<>settingName) $ withBackedDevice $ \dev dl->do
        value <- param "value"
        result <- liftIO $ runEitherT $ do
            DL.set dl setting value
            when saveNV $ DL.saveNVConfig dl
        case result of
          Left err -> do status status500
                         text $ "Error: "<>TL.pack err
          Right _  -> json $ JSON.object [ "setting"  .= settingName
                                         , "deviceId" .= devId dev
                                         , "value"    .= toJSON value
                                         ]
 
getPutSetting :: (ToJSON a, Parsable a)
              => String -> DL.Setting a -> ScottyM ()
getPutSetting name setting = do
    getSetting name setting              
    putSetting name setting False

getPutNVSetting :: (ToJSON a, Parsable a)
                => String -> DL.Setting a -> ScottyM ()
getPutNVSetting name setting = do
    getSetting name setting
    putSetting name setting True

instance ToJSON DL.Sample where
    toJSON s =
      JSON.object [ "time"   .= DL.sampleTime s
                  , "sensor" .= case DL.sampleSensor s of DL.SID n -> n
                  , "value"  .= DL.sampleValue s
                  ]

csv :: Csv.ToRecord a => [a] -> ActionM ()
csv xs = do
    setHeader "Content-Type" "text/plain"
    raw $ Csv.encode xs

getSamplesAction :: Device -> Maybe DL.SensorId
                 -> (V.Vector Sample -> ActionM ()) -> ActionM ()
getSamplesAction dev sensor format = do
    let filterSensor = maybe id (\sensor->V.filter (\s->DL.sampleSensor s == sensor)) sensor
    result <- lift (fetch dev)
    case result of 
      (samples, Nothing) -> format (filterSensor samples)
      (samples, Just (FetchProgress done total)) -> do
          addHeader "X-Samples-Done" (TL.pack $ show done)
          addHeader "X-Samples-Total" (TL.pack $ show total)
          
          format (filterSensor samples)
          status status202

withLoggerResult :: EitherT String IO a -> (a -> ActionM ()) -> ActionM ()
withLoggerResult loggerAction go = do
    result <- liftIO $ runEitherT loggerAction
    case result of
      Left error   -> do html ("<h1>Error</h1><p>"<>TL.pack error<>"</p>")
                         status status500
      Right result -> go result

routes :: ScottyM () 
routes = do
    get "/devices" $ do
        lift getDeviceList >>= json . map devId
    post "/devices" $ do
        lift refreshDevices
        lift getDeviceList >>= json . map devId

    post "/devices/add-test" $ do
        lift $ runEitherT $ addTestDevice (DN "hello")
        status status200

    getPutSetting "acquiring" DL.acquiring
    getPutSetting "sample-period" DL.samplePeriod
    getPutSetting "rtc-time" DL.rtcTime
    getPutNVSetting "acquire-on-boot" DL.acquireOnBoot
    getPutNVSetting "name" DL.deviceName
    
    post "/devices/:device/erase" $ withBackedDevice $ \dev dl->do
        withLoggerResult (DL.resetSampleCount dl) $ \_->do
            json $ JSON.object [("success", toJSON True)]

    post "/devices/:device/eject" $ withDevice $ \dev->do
        -- TODO
        json $ JSON.object [("success", toJSON True)]
        
    get "/devices/:device/sample_count" $ withDevice $ \dev->do
        count <- getSampleCount dev
        json $ JSON.object ["value" .= count]
    
    get "/devices/:device/samples/csv" $ withDevice $ \dev->
        getSamplesAction dev Nothing (csv . V.toList)

    get "/devices/:device/samples/json" $ withDevice $ \dev->
        getSamplesAction dev Nothing json

    get "/devices/:device/sensors" $ withBackedDevice $ \dev dl->do
        withLoggerResult (DL.getSensors dl) $ \sensors->do
            json sensors

    get "/devices/:device/sensors/:sensor/samples/csv" $ withDevice $ \dev->do
        sensor <- param "sensor"
        getSamplesAction dev (Just sensor) (csv . V.toList)

    get "/devices/:device/sensors/:sensor/samples/json" $ withDevice $ \dev->do
        sensor <- param "sensor"
        getSamplesAction dev (Just sensor) json

    get "/" $ file "index.html"
    get "/logo.svg" $ file "logo.svg"
    get "/jquery.js" $ file "jquery-2.0.3.js"
    get "/ui.js" $ file "ui.js"
    get "/app.css" $ file "app.css"
    get "/app.js" $ file "app.js"
    get "/chart.css" $ file "chart.css"
    get "/chart.js" $ file "chart.js"

