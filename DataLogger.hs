{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ExistentialQuantification #-}

module DataLogger ( findDataLoggers
                  , DataLogger
                  , open
                    -- * Simple queries
                  , getVersion
                  , getSampleCount
                  , getDeviceId
                  , DeviceId(..)
                  , SensorId(..)
                  , MeasurableId(..)
                  , Sensor(..)
                  , getSensors
                  , Measurable(..)
                  , getMeasurables
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
import Data.Attoparsec.Char8
import Data.Monoid
import Data.List as L
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Error
import Data.EitherR (fmapLT)
import Control.Monad (void, when, forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import System.FilePath       
import System.Hardware.Serialport
import System.IO       
import Data.List (isSuffixOf, intercalate)
import System.Directory (getDirectoryContents)       
import System.FilePath ((</>))       
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Csv as Csv
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)

-- | Find possible data logger devices       
findDataLoggers :: IO [FilePath]
findDataLoggers = do
    let root = "/dev"                
    devices <- getDirectoryContents root
    return $ map (root </>) $ filter isACM devices
  where
    isACM fname = any (`isPrefixOf` takeFileName fname) ["ttyACM", "tty.usb"]

data CmdReq = forall a. CmdReq Command (Parser a) (TMVar (Either String [a]))

data DataLogger = DataLogger { handle  :: Handle
                             , request :: TQueue CmdReq
                             }

open :: MonadIO m => FilePath -> EitherT String m DataLogger
open device = do
    h <- liftIO $ hOpenSerial device defaultSerialSettings
    reqQueue <- liftIO newTQueueIO
    let dl = DataLogger h reqQueue
    liftIO $ forkIO $ ioWorker device dl
    -- Make sure things are working properly
    v <- getVersion dl
    return dl

tryIOStr :: IO a -> EitherT String IO a
tryIOStr = fmapLT show . tryIO

readReply :: Handle -> EitherT String IO BS.ByteString
readReply h = go BS.empty
  where
    go bs = do
      l <- tryIOStr $ BS.hGetLine h
      if BS.null l
        then return bs
        else go $ bs<>l<>"\n"

ioWorker :: FilePath -> DataLogger -> IO ()
ioWorker device (DataLogger h req) = loop
  where
    loop = do
      CmdReq cmd replyParser replyVar <- liftIO $ atomically $ readTQueue req
      liftIO $ putStr $ L.take 10 (BS.unpack cmd++repeat ' ')++"   ==>   "
      reply <- liftIO $ runEitherT $ do
          tryIOStr $ BS.hPutStrLn h cmd
          reply <- readReply h
          liftIO $ BS.putStr $ BS.take 30 reply
          hoistEither $ parseOnly (parseReply replyParser) reply
      liftIO $ putStrLn $ either errorMsg (const "") reply
      atomically $ putTMVar replyVar reply
      case reply of
        Left _  -> hClose h
        Right _ -> loop

    errorMsg err = device++": communication error: "++err
    parseReply :: Parser a -> Parser [a]
    parseReply parser = many $ parser <* endOfLine
    
close :: MonadIO m => DataLogger -> m ()
close (DataLogger h _) = liftIO $ hClose h

type Command = ByteString
     
command :: MonadIO m => DataLogger -> Command -> Parser a -> EitherT String m [a]
command (DataLogger _ req) cmd replyParser = do
    replyVar <- liftIO $ atomically $ do
        replyVar <- newEmptyTMVar
        writeTQueue req $ CmdReq cmd replyParser replyVar
        return replyVar
    EitherT $ liftIO $ atomically $ takeTMVar replyVar

type Key = ByteString

keyValue :: Parser Key -> Parser a -> Parser (Key, a)
keyValue keyParser valueParser = do
    key <- keyParser
    skipSpace
    char '='
    skipSpace
    value <- valueParser
    return (key, value)

valueCommand :: MonadIO m => DataLogger -> Command -> Key
             -> Parser a -> EitherT String m a
valueCommand dl cmd key valueParser = do
    ret <- command dl cmd $ keyValue (string key) valueParser
    case ret of
      [(_,v)] -> right v
      []      -> left $ "Not enough responses to "<>BS.unpack cmd
      _       -> left $ "Too many responses to "<>BS.unpack cmd

getVersion :: MonadIO m => DataLogger -> EitherT String m String
getVersion dl = valueCommand dl "V" "version" $ many hexDigit

getSampleCount :: MonadIO m => DataLogger -> EitherT String m Int
getSampleCount dl = valueCommand dl "n" "sample count" $ fromIntegral <$> decimal

forceSample :: MonadIO m => DataLogger -> EitherT String m ()
forceSample dl = void $ command dl "f" $ return ()

data Sensor = Sensor { sensorId   :: SensorId
                     , sensorName :: String
                     }
            deriving (Show)
           
name :: Parser String
name = (many $ alphaNum <|> satisfy (inClass "-_ ")) <?> "Token"

getSensors :: MonadIO m
           => DataLogger -> EitherT String m [Sensor]
getSensors dl = do
    command dl "s" $ Sensor <$> parseSensorId <* char '\t'
                            <*> name          <* char '\t'

data Measurable = Measurable { measurableId :: MeasurableId
                             , measurableName :: String
                             , measurableUnit :: String
                             }
                deriving (Show)
                
getMeasurables :: MonadIO m
               => DataLogger -> SensorId -> EitherT String m [Measurable]
getMeasurables dl (SID sid) = do
    command dl (BS.pack $ "m "<>show sid)
    $ Measurable <$> parseMeasurableId <* char '\t'
                 <*> name              <* char '\t'
                 <*> name

alphaNum :: Parser Char
alphaNum = letter_ascii <|> digit
         
hexDigit :: Parser Char
hexDigit = satisfy $ inClass "0123456789abcdefABCDEF"

getLastSamples :: MonadIO m => DataLogger -> EitherT String m [Sample]
getLastSamples dl = command dl "l" $ parseSample

newtype DeviceId = DevId String    
                 deriving (Show, Eq, Ord)

parseDeviceId :: Parser DeviceId
parseDeviceId = DevId <$> many (hexDigit <|> char '-') <?> "device ID"

getDeviceId :: MonadIO m => DataLogger -> EitherT String m DeviceId
getDeviceId dl = valueCommand dl "I" "device id" parseDeviceId

newtype SensorId = SID Int
                 deriving (Show, Ord, Eq, Csv.ToField)

parseSensorId :: Parser SensorId
parseSensorId = (SID . fromIntegral) <$> decimal <?> "sensor ID"

newtype MeasurableId = MID Int
                 deriving (Show, Ord, Eq, Csv.ToField)

parseMeasurableId :: Parser MeasurableId
parseMeasurableId = (MID . fromIntegral) <$> decimal <?> "measurable ID"

data Sample = Sample { sampleTime       :: Integer
                     , sampleSensor     :: SensorId
                     , sampleMeasurable :: MeasurableId
                     , sampleValue      :: Float
                     }
            deriving (Show)

instance Csv.ToRecord Sample where
    toRecord s = Csv.record [ Csv.toField (sampleTime s)
                            , Csv.toField (sampleSensor s)
                            , Csv.toField (sampleMeasurable s)
                            , Csv.toField (sampleValue s)
                            ]

getSamples :: MonadIO m => DataLogger -> Int -> Int -> EitherT String m [Sample]
getSamples dl start count = do
    let cmd = BS.pack $ intercalate " " ["g", show start, show count]
    command dl cmd $ parseSample

parseSample :: Parser Sample
parseSample = p <?> "sample"
  where
    p = Sample <$> (signed decimal <?> "time") <*  skipSpace
               <*> parseSensorId               <*  skipSpace
               <*> parseMeasurableId           <*  skipSpace
               <*> (realToFrac <$> double <?> "value")

resetSampleCount :: MonadIO m => DataLogger -> EitherT String m ()
resetSampleCount dl = void $ valueCommand dl "n!" "sample count" $ decimal

showBool :: Bool -> ByteString
showBool True  = "1"
showBool False = "0"
         
parseBool :: Parser Bool
parseBool = (char '1' >> return True) <|> (char '0' >> return False)
            <?> "boolean"

saveNVConfig :: MonadIO m => DataLogger -> EitherT String m ()
saveNVConfig dl = void $ command dl "NS" $ return ()

data Setting a = Setting { sCommand :: Command
                         , sName    :: ByteString
                         , sParse   :: Parser a
                         , sShow    :: a -> ByteString
                         }
               
set :: MonadIO m => DataLogger -> Setting a -> a -> EitherT String m ()
set dl s value = do
    let cmd = sCommand s<>"="<>sShow s value
    void $ valueCommand dl cmd (sName s) (sParse s)
    
get :: MonadIO m => DataLogger -> Setting a -> EitherT String m a
get dl s = do
    valueCommand dl (sCommand s) (sName s) (sParse s)

-- | Sample period in milliseconds    
samplePeriod :: Setting Int
samplePeriod =
    Setting { sCommand = "T"
            , sName    = "sample period"
            , sParse   = fromInteger <$> decimal
            , sShow    = BS.pack . show
            }
             
rtcTime :: Setting Int
rtcTime =
    Setting { sCommand = "t"
            , sName    = "RTC time"
            , sParse   = fromInteger <$> decimal
            , sShow    = BS.pack . show
            }
    
acquiring :: Setting Bool
acquiring =
    Setting { sCommand = "a"
            , sName    = "acquiring"
            , sParse   = parseBool
            , sShow    = showBool
            }

acquireOnBoot :: Setting Bool
acquireOnBoot =
    Setting { sCommand = "NB"
            , sName    = "acquire on boot"
            , sParse   = parseBool
            , sShow    = showBool
            }

deviceName :: Setting String
deviceName =
    Setting { sCommand = "NN"
            , sName    = "device name"
            , sParse   = name
            , sShow    = BS.pack
            }
