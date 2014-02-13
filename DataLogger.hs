{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}

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
import Text.Trifecta
import Data.Monoid
import Data.Char (isDigit)
import Data.List as L
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Error
import Data.EitherR (fmapLT)
import Control.Monad (void, when, forever)
import Control.Monad.IO.Class
import System.Hardware.Serialport
import System.IO       
import Data.List (isSuffixOf, intercalate)
import System.Directory (getDirectoryContents)       
import System.FilePath ((</>))       
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Csv as Csv
import qualified Data.ByteString as BS

-- | Find possible data logger devices       
findDataLoggers :: IO [FilePath]
findDataLoggers = do
    let root = "/dev"                
    devices <- getDirectoryContents root
    return $ map (root </>) $ filter isACM devices
  where
    isACM = isSuffixOf "ttyACM" . reverse . dropWhile isDigit . reverse

data CmdReq = forall a. CmdReq Command (Parser a) (TMVar (Either String [a]))

data DataLogger = DataLogger { handle  :: Handle
                             , request :: TQueue CmdReq
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

tryIOStr :: IO a -> EitherT String IO a
tryIOStr = fmapLT show . tryIO

ioWorker :: DataLogger -> IO ()
ioWorker (DataLogger h req) = forever $ do
    CmdReq cmd replyParser replyVar <- atomically $ readTQueue req
    putStr $ L.take 10 (cmd++repeat ' ') ++ "   =>   "
    reply <- runEitherT $ do
        tryIOStr $ hPutStr h (cmd <> "\n")
        l <- tryIOStr $ BS.hGet h 1
        go $ stepParser (parseReply replyParser) mempty l
    either (\err->putStrLn $ "parse error\n"++err) (const $ putStrLn "") reply
    atomically $ putTMVar replyVar reply
  where
    go (StepDone _ xs)     = right xs
    go (StepFail _ error)  = left (show error)
    go step                = do
        l <- tryIOStr $ BS.hGet h 1
        liftIO $ print l
        go $ feed l step
    parseReply :: Parser a -> Parser [a]
    parseReply parser = manyTill (parser <* newline) (newline >> eof)
    
close :: MonadIO m => DataLogger -> m ()
close (DataLogger h _) = liftIO $ hClose h

type Command = String
     
command :: MonadIO m => DataLogger -> Command -> Parser a -> EitherT String m [a]
command (DataLogger _ req) cmd replyParser = do
    replyVar <- liftIO $ atomically $ do
        replyVar <- newEmptyTMVar
        writeTQueue req $ CmdReq cmd replyParser replyVar
        return replyVar
    EitherT $ liftIO $ atomically $ takeTMVar replyVar

type Key = String

keyValue :: Parser Key -> Parser a -> Parser (Key, a)
keyValue keyParser valueParser = do
    key <- keyParser
    spaces
    char '='
    spaces
    value <- valueParser
    return (key, value)

valueCommand :: MonadIO m => DataLogger -> Command -> String
             -> Parser a -> EitherT String m a
valueCommand dl cmd key valueParser = do
    ret <- command dl cmd $ keyValue (string key) valueParser
    case ret of
      [(_,v)] -> right v
      []      -> left $ "Not enough responses to "<>cmd
      _       -> left $ "Too many responses to "<>cmd

getVersion :: MonadIO m => DataLogger -> EitherT String m String
getVersion dl = valueCommand dl "V" "version" $ many hexDigit

getSampleCount :: MonadIO m => DataLogger -> EitherT String m Int
getSampleCount dl = valueCommand dl "n" "sample count" $ fromIntegral <$> decimal

forceSample :: MonadIO m => DataLogger -> EitherT String m ()
forceSample dl = void $ command dl "f" $ return ()

data Sensor = Sensor { sensorId   :: SensorId
                     , sensorName :: String
                     , sensorUnit :: String
                     }
            deriving (Show)
           
name :: Parser String
name = (many $ alphaNum <|> digit <|> oneOf "-_ ") <?> "Token"

parseSensorId :: Parser SensorId
parseSensorId = (SID . fromIntegral) <$> decimal <?> "Sensor ID"

getSensors :: MonadIO m => DataLogger -> EitherT String m [Sensor]
getSensors dl = do
    command dl "s" $ Sensor <$> parseSensorId <* spaces <* char '\t'
                            <*> name          <* spaces <* char '\t'
                            <*> name          <* spaces

getLastSamples :: MonadIO m => DataLogger -> EitherT String m [Sample]
getLastSamples dl = command dl "l" $ parseSample

newtype DeviceId = DevId String    
                 deriving (Show, Eq, Ord)

parseDeviceId :: Parser DeviceId
parseDeviceId = DevId <$> many (hexDigit <|> char '-') <?> "Device ID"

getDeviceId :: MonadIO m => DataLogger -> EitherT String m DeviceId
getDeviceId dl = valueCommand dl "I" "device id" parseDeviceId

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
    command dl cmd $ parseSample

parseSample :: Parser Sample
parseSample = do
    Sample <$> (decimal <?> "time") <*  spaces
           <*> parseSensorId        <*  spaces
           <*> (realToFrac <$> double <?> "value")

resetSampleCount :: MonadIO m => DataLogger -> EitherT String m ()
resetSampleCount dl = void $ valueCommand dl "n!" "sample count" $ decimal

showBool :: Bool -> String
showBool True  = "1"
showBool False = "0"
         
parseBool :: Parser Bool
parseBool = (char '1' >> return True) <|> (char '0' >> return False)
            <?> "Boolean"

saveNVConfig :: MonadIO m => DataLogger -> EitherT String m ()
saveNVConfig dl = void $ command dl "NS" $ return ()

data Setting a = Setting { sCommand :: String
                         , sName    :: String
                         , sParse   :: Parser a
                         , sShow    :: a -> String
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
            , sShow    = show
            }
             
rtcTime :: Setting Int
rtcTime =
    Setting { sCommand = "t"
            , sName    = "RTC time"
            , sParse   = fromInteger <$> decimal
            , sShow    = show
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
            , sShow    = id
            }
