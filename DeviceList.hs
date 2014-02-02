{-# LANGUAGE GeneralizedNewtypeDeriving #-}                

module DeviceList ( -- * Device type
                    DeviceName(..)
                  , Device(devId)
                  , devLogger
                  , getSampleCount
                    -- * Device list
                  , DeviceList
                  , newDeviceList
                    -- * Device list monad
                  , DeviceListT
                  , runDeviceListT
                  , getDeviceList
                  , withDeviceList
                  , refreshDevices
                  , lookupDeviceId
                  , fetch
                  , FetchProgress(..)
                    -- * Testing
                  , addTestDevice
                  ) where                

import Control.Error
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import qualified Data.Vector as V       
import qualified Data.Map as M
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent
import Control.Concurrent.STM       
import Control.Monad (when, forM_, void, liftM, forever)
import Data.Maybe (mapMaybe)

import DataLogger (DataLogger, DeviceId, Sample)
import qualified DataLogger as DL

newtype DeviceName = DN String
                   deriving (Show, Eq, Ord, ToJSON, FromJSON)

newtype DeviceList = DL { getDL :: TVar (M.Map DeviceId Device) }

newtype DeviceListT m a = DLT (ReaderT DeviceList m a)
                        deriving ( MonadTrans, MonadIO, Monad
                                 , Applicative, Functor)

newDeviceList :: MonadIO m => m DeviceList
newDeviceList = liftIO $ DL <$> newTVarIO M.empty

runDeviceListT :: (MonadIO m) => DeviceList -> DeviceListT m a -> m a
runDeviceListT devList (DLT m) = do
    runReaderT m devList

lookupDeviceId :: MonadIO m => DeviceId -> DeviceListT m (Maybe Device)
lookupDeviceId devId = withDeviceMap $ return . M.lookup devId

withDeviceMap :: MonadIO m => (M.Map DeviceId Device -> DeviceListT m a) -> DeviceListT m a
withDeviceMap m = getDeviceMap >>= m

withDeviceList :: MonadIO m => ([Device] -> DeviceListT m a) -> DeviceListT m a
withDeviceList m = getDeviceList >>= m

getDeviceList :: MonadIO m => DeviceListT m [Device]
getDeviceList = M.elems `liftM` getDeviceMap              

getDeviceMap :: MonadIO m => DeviceListT m (M.Map DeviceId Device)
getDeviceMap = liftIO . atomically . readTVar . getDL =<< DLT ask

modifyDeviceMap :: MonadIO m
                => (M.Map DeviceId Device -> M.Map DeviceId Device)
                -> DeviceListT m ()
modifyDeviceMap f = do
    DL devMap <- DLT ask
    liftIO $ atomically $ modifyTVar devMap f

data DeviceBackend = TestDevice
                   | LocalDevice DataLogger FilePath

data Device = Device { devBackend :: DeviceBackend -- ^ Device backend
                     , devId      :: DeviceId      -- ^ Unique @DeviceId@
                     , devSamples :: TVar (V.Vector Sample)
                     , devSampleCount :: TVar Int
                     }

removeDevice :: MonadIO m => DeviceId -> DeviceListT m ()
removeDevice devId = modifyDeviceMap $ M.delete devId

refreshDevices :: MonadIO m => DeviceListT m ()
refreshDevices = do
    loggers <- liftIO DL.findDataLoggers
    knownDevices <- withDeviceList return
    
    -- Make sure no devices have disappeared
    --forM_ knownDevices $ \dev->do
    --    when (devPath dev `notElem` loggers)
    --      $ removeDevice (devId dev)

    -- Add new devices
    let devPath (Device {devBackend = LocalDevice _ dev}) = Just dev
        devPath _                                         = Nothing
        newDevs = filter (`notElem` mapMaybe devPath knownDevices) loggers
    forM_ newDevs $ \devPath->runEitherT $ addLocalDevice devPath

addLocalDevice :: MonadIO m
               => FilePath -> EitherT String (DeviceListT m) DeviceId
addLocalDevice devPath = do
    dl <- DL.open devPath
    EitherT $ liftIO $ runEitherT $ checkRTCTime dl
    devId <- DL.getDeviceId dl
    samples <- liftIO $ newTVarIO V.empty
    sampleCount <- liftIO $ newTVarIO 0
    let dev = Device { devBackend     = LocalDevice dl devPath
                     , devId          = devId
                     , devSamples     = samples
                     , devSampleCount = sampleCount
                     }
    liftIO $ forkIO $ void $ runEitherT $ fetchWorker dev
    lift $ modifyDeviceMap $ M.insert devId dev
    return devId
    
addTestDevice :: MonadIO m
              => DeviceName -> EitherT String (DeviceListT m) DeviceId
addTestDevice name = do
    let devId = DL.DevId "test"
    samples <- liftIO $ newTVarIO V.empty
    sampleCount <- liftIO $ newTVarIO 0
    let dev = Device { devBackend     = TestDevice
                     , devId          = devId
                     , devSamples     = samples
                     , devSampleCount = sampleCount
                     }
    liftIO $ forkIO $ void $ runEitherT $ fetchTestWorker dev
    lift $ modifyDeviceMap $ M.insert devId dev
    return devId
    
fetchTestWorker :: MonadIO m => Device -> EitherT String m ()
fetchTestWorker dev = forever $ do
    realTime <- round . realToFrac <$> liftIO getPOSIXTime
    liftIO $ threadDelay (100 * 1000)
    liftIO $ threadDelay (5 * 1000 * 1000)
    liftIO $ atomically $ modifyTVar (devSampleCount dev) (+1)
    let s = DL.Sample realTime (DL.SID 1) 1
    liftIO $ atomically $ modifyTVar (devSamples dev) (`V.snoc` s)

devLogger :: Device -> Maybe DataLogger
devLogger dev
  | LocalDevice dl _ <- devBackend dev = Just dl
  | otherwise                          = Nothing
  
fetchWorker :: MonadIO m => Device -> EitherT String m ()
fetchWorker dev
  | Nothing <- devLogger dev  = error "fetchWorker on un-backed Device"
  | Just dl <- devLogger dev  = forever $ do
    count <- DL.getSampleCount dl
    liftIO $ atomically $ writeTVar (devSampleCount dev) count
    fetchUpTo dl count
    liftIO $ threadDelay (5 * 1000* 1000)
  where
    chunkSz = 100
    fetchUpTo :: MonadIO m => DataLogger -> Int -> EitherT String m ()
    fetchUpTo dl count = do
        n <- liftIO $ atomically $ V.length `fmap` readTVar (devSamples dev)
        case () of
          -- Still have more samples to fetch
          _ | n < count -> do
                let fetchCount = min (count - n) chunkSz 
                chunk <- V.fromList <$> DL.getSamples dl n fetchCount
                liftIO $ atomically $ modifyTVar (devSamples dev)
                       $ (V.++ chunk)
                fetchUpTo dl count
          -- Sample count has decreased, reset sample cache
            | n > count -> do
                liftIO $ atomically $ writeTVar (devSamples dev) V.empty
                fetchUpTo dl count
          -- We have all of the available samples
            | otherwise -> return ()

data FetchProgress = FetchProgress { progressDone, progressTotal :: Int }
                   deriving (Show)
                   
fetch :: MonadIO m
      => Device -> DeviceListT m (V.Vector Sample, Maybe FetchProgress)
fetch dev = liftIO $ atomically $ do 
    samples <- readTVar (devSamples dev)
    count <- readTVar (devSampleCount dev)
    let progress = if V.length samples < count
                     then Just $ FetchProgress (V.length samples) count
                     else Nothing
    return (samples, progress)

getSampleCount :: MonadIO m => Device -> m Int
getSampleCount = liftIO . atomically . readTVar . devSampleCount               

-- | Check that the RTC time has been set    
checkRTCTime :: DataLogger -> EitherT String IO ()
checkRTCTime dl = do
    t <- DL.get dl DL.rtcTime
    when (t == 0) $ do
      realTime <- liftIO getPOSIXTime
      DL.set dl DL.rtcTime (round $ realToFrac realTime)
