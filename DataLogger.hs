{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataLogger ( findDataLoggers
                  , DataLogger
                  , open
                    -- * Simple queries
                  , getVersion
                  , getSampleCount
                  , getDeviceId
                  , DeviceId(..)
                  , getSensors
                  , SensorId(..)
                  , Sensor(..)
                  , getLastSamples
                    -- * Fetching samples
                  , forceSample
                  , Sample(..)
                  , getSamples
                  , resetSampleCount
                    -- * Settings
                  , Setting
                  , get
                  , set
                  , samplePeriod
                  , rtcTime
                  , acquiring
                  , acquireOnBoot
                  , deviceName
                    -- * Non-volatile configuration
                  , saveNVConfig
                  ) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Error
import Control.Monad (void, when, forever)
import Control.Monad.IO.Class
import System.Hardware.Serialport
import System.IO       
import Data.Char (isSpace, isDigit)
import Data.List (isSuffixOf, intercalate)
import System.Directory (getDirectoryContents)       
import System.FilePath ((</>))       
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Csv as Csv

-- | Find possible data logger devices       
findDataLoggers :: IO [FilePath]
findDataLoggers = do
    let root = "/dev"                
    devices <- getDirectoryContents root
    return $ map (root </>) $ filter isACM devices
  where
    isACM = isSuffixOf "ttyACM" . reverse . dropWhile isDigit . reverse

data DataLogger = DataLogger { handle  :: Handle
                             , request :: TQueue (String, TMVar [String])
                             }

open :: MonadIO m => FilePath -> EitherT String m DataLogger
open device = do
    h <- liftIO $ hOpenSerial device defaultSerialSettings
    reqQueue <- liftIO newTQueueIO
    let dl = DataLogger h reqQueue
    liftIO $ forkIO $ ioWorker dl
    -- Make sure things are working properly
    v <- getVersion dl
    return dl
    
ioWorker :: DataLogger -> IO ()
ioWorker (DataLogger h req) = forever $ do
    (cmd,replyVar) <- atomically $ readTQueue req
    liftIO $ hPutStr h (cmd ++ "\n")
    reply <- go []
    --print (cmd, reply)
    print cmd
    atomically $ putTMVar replyVar reply
  where
    go ls = do l <- strip `fmap` liftIO (hGetLine h)
               if l == ""
                 then return (reverse ls)
                 else go (l:ls)
    
close :: MonadIO m => DataLogger -> m ()
close (DataLogger h _) = liftIO $ hClose h

type Command = String
     
command :: MonadIO m => DataLogger -> Command -> EitherT String m [String]
command (DataLogger _ req) cmd = liftIO $ do
    reply <- atomically $ do reply <- newEmptyTMVar
                             writeTQueue req (cmd, reply)
                             return reply
    atomically $ takeTMVar reply

-- | Read a single-line reply 
simpleCommand :: MonadIO m => DataLogger -> Command -> EitherT String m String
simpleCommand dl cmd = do
    ls <- command dl cmd
    case ls of
      [l] -> return l
      []  -> left "Expected simple reply, recieved nothing"
      _   -> left "Expected simple reply, recieved multi-line reply"
   
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace 

split :: Char -> String -> Maybe (String, String)
split c xs
  | null b    = Nothing
  | otherwise = Just (a, b)
  where
    a = takeWhile (/= c) xs
    b = emptyTail $ dropWhile (/= c) xs
    emptyTail :: [a] -> [a]
    emptyTail []     = []
    emptyTail (x:xs) = xs

valuesCommand :: MonadIO m => DataLogger -> Command -> EitherT String m (M.Map String String)
valuesCommand dl cmd = do
    M.fromList . catMaybes . map (fmap stripKeyValue . split '=') <$> command dl cmd
  where
    stripKeyValue (a, b) = (strip a, strip b)

valueCommand :: MonadIO m => DataLogger -> Command -> String -> EitherT String m String
valueCommand dl cmd key = do
    reply <- valuesCommand dl cmd
    case M.lookup key reply of
      Nothing  -> left ("Key "++key++" not found")
      Just val -> right val

getVersion :: MonadIO m => DataLogger -> EitherT String m String
getVersion dl = valueCommand dl "V" "version"

getSampleCount :: MonadIO m => DataLogger -> EitherT String m Int
getSampleCount dl = read <$> valueCommand dl "n" "sample count"

forceSample :: MonadIO m => DataLogger -> EitherT String m ()
forceSample dl = void $ command dl "f"

data Sensor = Sensor { sensorId   :: SensorId
                     , sensorName :: String
                     , sensorUnit :: String
                     }
            deriving (Show)
            
getSensors :: MonadIO m => DataLogger -> EitherT String m [Sensor]
getSensors dl = do
    reply <- command dl "s"
    let (errors, sensors) = partitionEithers $ map parseSensor reply
    return sensors
  where
    parseSensor l =
      case words l of
        [sensorId, name, unit] -> Sensor <$> SID `fmap` readErr "Error parsing sensor ID"  sensorId
                                         <*> pure name
                                         <*> pure unit
        _                      -> Left "Parse error"

getLastSamples :: MonadIO m => DataLogger -> EitherT String m [Sample]
getLastSamples dl = do
    reply <- command dl "l"
    let (errors, samples) = partitionEithers $ map parseSample reply
    when (not $ null errors) $ liftIO $ print errors
    return samples

newtype DeviceId = DevId String    
                 deriving (Show, Eq, Ord)

getDeviceId :: MonadIO m => DataLogger -> EitherT String m DeviceId
getDeviceId dl = DevId <$> valueCommand dl "I" "device id"

newtype SensorId = SID Int
                 deriving (Show, Ord, Eq, Csv.ToField)

data Sample = Sample { sampleTime   :: Integer
                     , sampleSensor :: SensorId
                     , sampleValue  :: Float
                     }
            deriving (Show)

instance Csv.ToRecord Sample where
    toRecord s = Csv.record [ Csv.toField (sampleTime s)
                            , Csv.toField (sampleSensor s)
                            , Csv.toField (sampleValue s)
                            ]

getSamples :: MonadIO m => DataLogger -> Int -> Int -> EitherT String m [Sample]
getSamples dl start count = do
    let cmd = intercalate " " ["g", show start, show count]
    reply <- command dl cmd
    let (errors, samples) = partitionEithers $ map parseSample reply
    when (not $ null errors) $ liftIO $ print errors
    return $ filter (\s->sampleTime s > 0) samples
    
parseSample :: String -> Either String Sample
parseSample l =
  case words l of
    [time, sid, val]  ->
        Sample <$> readErr "Error parsing time" time
               <*> fmap SID (readErr "Error parsing sensor ID" sid)
               <*> readErr "Error parsing value" val
    _                 ->
        Left "getSamples: Not enough fields"

resetSampleCount :: MonadIO m => DataLogger -> EitherT String m ()
resetSampleCount dl = void $ valueCommand dl "n!" "sample count"

showBool :: Bool -> String
showBool True  = "1"
showBool False = "0"
         
readBool :: String -> Either String Bool
readBool "1" = Right True
readBool "0" = Right False
readBool  a  = Left ("Unexpected boolean value: "++a)
          
saveNVConfig :: MonadIO m => DataLogger -> EitherT String m ()
saveNVConfig dl = void $ command dl "S"          

data Setting a = Setting { sCommand :: String
                         , sName    :: String
                         , sRead    :: String -> Either String a
                         , sShow    :: a -> String
                         }
               
set :: MonadIO m => DataLogger -> Setting a -> a -> EitherT String m ()
set dl s value = do
    let cmd = sCommand s++"="++sShow s value
    void $ valueCommand dl cmd (sName s)
    
get :: MonadIO m => DataLogger -> Setting a -> EitherT String m a
get dl s = do
    reply <- valueCommand dl (sCommand s) (sName s)
    hoistEither $ sRead s reply

-- | Sample period in milliseconds    
samplePeriod :: Setting Int
samplePeriod =
    Setting { sCommand = "T"
            , sName    = "sample period"
            , sRead    = pure . read
            , sShow    = show
            }
             
rtcTime :: Setting Int
rtcTime =
    Setting { sCommand = "t"
            , sName    = "RTC time"
            , sRead    = pure . read
            , sShow    = show
            }
    
acquiring :: Setting Bool
acquiring =
    Setting { sCommand = "a"
            , sName    = "acquiring"
            , sRead    = readBool
            , sShow    = showBool
            }

acquireOnBoot :: Setting Bool
acquireOnBoot =
    Setting { sCommand = "B"
            , sName    = "acquire on boot"
            , sRead    = readBool
            , sShow    = showBool
            }

deviceName :: Setting String
deviceName =
    Setting { sCommand = "N"
            , sName    = "device name"
            , sRead    = Right . id
            , sShow    = id
            }
