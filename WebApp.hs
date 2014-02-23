{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TypeSynonymInstances #-}
                
import Data.Monoid       
import Data.Foldable       
import Data.Traversable hiding (mapM)
import Control.Applicative ((<$>))
import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Concurrent.STM       
import qualified Data.Map as M

import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Csv as Csv
import Control.Error

import Network.HTTP.Types.Status (status200, status202, status500, status404)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=))
import qualified Data.Aeson as JSON
import Web.Scotty.Trans hiding (ScottyM, ActionM)

import DataLogger (DataLogger, DeviceId, Sample, SensorId, MeasurableId)
import qualified DataLogger as DL
import DeviceList
import Average

deriving instance Parsable DeviceId
deriving instance Parsable DeviceName
deriving instance Parsable DL.SensorId
deriving instance Parsable DL.MeasurableId

deriving instance ToJSON DeviceId
deriving instance ToJSON DL.SensorId
deriving instance ToJSON DL.MeasurableId

instance ToJSON DL.Sensor where
    toJSON s = JSON.object [ "sensor_id" .= DL.sensorId s
                           , "name"      .= DL.sensorName s
                           ]

instance ToJSON DL.Measurable where
    toJSON s = JSON.object [ "measurable_id" .= DL.measurableId s
                           , "name"          .= DL.measurableName s
                           , "unit"          .= DL.measurableUnit s
                           ]

type ScottyM = ScottyT TL.Text (DeviceListT IO)
type ActionM = ActionT TL.Text (DeviceListT IO)

data SensorMeasurables = SM DL.Sensor [DL.Measurable]

instance ToJSON SensorMeasurables where
    toJSON (SM s m) = JSON.object [ "sensor_id"   .= DL.sensorId s
                                  , "name"        .= DL.sensorName s
                                  , "measurables" .= m
                                  ]

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
                  , "measurable" .= case DL.sampleMeasurable s of DL.MID n -> n
                  , "value"  .= DL.sampleValue s
                  ]

csv :: Csv.ToRecord a => [a] -> ActionM ()
csv xs = do
    setHeader "Content-Type" "text/plain"
    raw $ Csv.encode xs

filterSensor :: DL.SensorId -> Sample -> Bool
filterSensor sensor s = DL.sampleSensor s == sensor

filterMeasurable :: DL.SensorId -> DL.MeasurableId -> Sample -> Bool
filterMeasurable sensor measurable s =
    DL.sampleSensor s == sensor && DL.sampleMeasurable s == measurable

data Pair a b = Pair a b
              deriving (Show)
            
instance (Monoid a, Monoid b) => Monoid (Pair a b) where
     mempty = Pair mempty mempty
     Pair a b `mappend` Pair x y = Pair (a <> x) (b <> y)

decimate :: Integer -> V.Vector Sample -> [Sample]
decimate res = go
  where
    averageMeasurables :: V.Vector Sample -> [Sample]
    averageMeasurables samples =
      let a :: M.Map (SensorId, MeasurableId) (Pair (Average Double) (Average Float))
          a = foldMap (\s->M.singleton (DL.sampleSensor s, DL.sampleMeasurable s)
                           $ Pair (average $ realToFrac $ DL.sampleTime s)
                                  (average $ DL.sampleValue s))
                      samples
          avgSamples = map (\((sid,mid), Pair t v)->DL.Sample (round $ getAverage t) sid mid (getAverage v))
                           (M.assocs a)
      in avgSamples
    go samples
      | V.null samples = []
      | DL.Sample {DL.sampleTime=startTime} <- V.head samples =
        let (ss, rest) = V.span (\s->startTime + res < DL.sampleTime s) samples
        in averageMeasurables ss ++ go rest

getSamplesAction :: Device -> (Sample -> Bool)
                 -> (V.Vector Sample -> ActionM ()) -> ActionM ()
getSamplesAction dev filterFn format = do
    result <- lift (fetch dev)
    case result of 
      (samples, Nothing) -> format $ V.filter filterFn samples
      (samples, Just (FetchProgress done total)) -> do
          addHeader "X-Samples-Done" (TL.pack $ show done)
          addHeader "X-Samples-Total" (TL.pack $ show total)
          
          format $ V.filter filterFn $ samples
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
        getSamplesAction dev (const True) (csv . V.toList)

    get "/devices/:device/samples/json" $ withDevice $ \dev->
        getSamplesAction dev (const True) json

    get "/devices/:device/sensors" $ withBackedDevice $ \dev dl->do
        withLoggerResult (DL.getSensors dl) $ \sensors->do
            json sensors

    get "/devices/:device/sensors/:sensor/measurables" $ withBackedDevice $ \dev dl->do
        sensor <- param "sensor"
        withLoggerResult (DL.getMeasurables dl sensor) $ \measurables->do
            json measurables

    get "/devices/:device/sensors/:sensor/samples/json" $ withBackedDevice $ \dev dl->do
        sensor <- param "sensor"
        withLoggerResult (DL.getMeasurables dl sensor) $ \measurables->do
            json measurables

    get "/devices/:device/sensors/:sensor/samples/csv" $ withDevice $ \dev->do
        sensor <- param "sensor"
        getSamplesAction dev (filterSensor sensor) (csv . V.toList)

    get "/devices/:device/sensors/:sensor/samples/json" $ withDevice $ \dev->do
        sensor <- param "sensor"
        getSamplesAction dev (filterSensor sensor) json

    get "/" $ file "index.html"
    get "/logo.svg" $ file "logo.svg"
    get "/jquery.js" $ file "jquery-2.0.3.js"
    get "/ui.js" $ file "ui.js"
    get "/app.css" $ file "app.css"
    get "/app.js" $ file "app.js"
    get "/chart.css" $ file "chart.css"
    get "/chart.js" $ file "chart.js"
    get "/d3.v3.js" $ file "d3.v3.min.js"

