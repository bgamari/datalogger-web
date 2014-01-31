{-# LANGUAGE GeneralizedNewtypeDeriving #-}                

module DeviceList ( -- * Device type
                    DeviceName(..)
                  , Device(devId, devLogger)
                    -- * Device list
                  , DeviceList
                  , newDeviceList
                    -- * Device list monad
                  , DeviceListT
                  , runDeviceListT
                  , getDeviceList
                  , withDeviceList
                  , refreshDevices
                  , filterDevicesByName
                  , lookupDeviceId
                  , fetch
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
import Control.Monad (when, forM_, void, liftM)

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

filterDevicesByName :: MonadIO m => DeviceName -> DeviceListT m [Device]
filterDevicesByName name =
    withDeviceMap $ return . filter (\dev->devName dev == name) . M.elems

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

data Device = Device { devName    :: DeviceName -- ^ Friendly name
                     , devPath    :: FilePath   -- ^ Path to serial device
                     , devLogger  :: DataLogger -- ^ @DataLogger@
                     , devId      :: DeviceId   -- ^ Unique @DeviceId@
                     , devSamples :: TVar (Either FetchProgress (V.Vector Sample))
                     }

removeDevice :: MonadIO m => DeviceId -> DeviceListT m ()
removeDevice devId = modifyDeviceMap $ M.delete devId

refreshDevices :: MonadIO m => DeviceListT m ()
refreshDevices = do
    loggers <- liftIO DL.findDataLoggers
    knownDevices <- withDeviceList return
    
    -- Make sure no devices have disappeared
    forM_ knownDevices $ \dev->do
        when (devPath dev `notElem` loggers)
          $ removeDevice (devId dev)

    -- Add new devices
    let newDevs = filter (`notElem` map devPath knownDevices) loggers
    forM_ newDevs $ \devPath->runEitherT $ addDevice devPath

addDevice :: MonadIO m
          => FilePath -> EitherT String (DeviceListT m) DeviceId
addDevice devPath = do
    dl <- DL.open devPath
    EitherT $ liftIO $ runEitherT $ checkRTCTime dl
    devList <- lift $ DLT ask
    devId <- DL.getDeviceId dl
    name <- DL.get dl DL.deviceName
    samples <- liftIO $ newTVarIO $ Left $ FetchProgress 0 1
    let dev = Device { devPath    = devPath
                     , devLogger  = dl
                     , devId      = devId
                     , devName    = DN name
                     , devSamples = samples
                     }
    lift $ startFetch dev
    lift $ modifyDeviceMap $ M.insert devId dev
    return devId

startFetch :: MonadIO m
           => Device -> DeviceListT m ()
startFetch dev = do
    _ <- liftIO $ forkIO $ void $ runEitherT doFetch
    return ()
  where
    doFetch = do
        count <- DL.getSampleCount (devLogger dev)
        let chunkSz = 100
        samples <- execWriterT $ forM_ [0,chunkSz..count] $ \i->do
              liftIO $ atomically $ writeTVar (devSamples dev)
                     $ Left (FetchProgress i count)
              chunk <- lift $ V.fromList <$> DL.getSamples (devLogger dev) 0 count
              tell chunk
        liftIO $ atomically $ writeTVar (devSamples dev) $ Right samples 

data FetchProgress = FetchProgress { progressDone, progressTotal :: Int }
                   deriving (Show)
                   
fetch :: MonadIO m
      => Device
      -> EitherT String (DeviceListT m) (Either FetchProgress (V.Vector Sample))
fetch dev = do      
    liftIO $ atomically $ readTVar (devSamples dev)

-- | Check that the RTC time has been set    
checkRTCTime :: DataLogger -> EitherT String IO ()
checkRTCTime dl = do
    t <- DL.get dl DL.rtcTime
    when (t == 0) $ do
      realTime <- liftIO getPOSIXTime
      DL.set dl DL.rtcTime (round $ realToFrac realTime)
