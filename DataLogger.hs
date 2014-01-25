module DataLogger ( findDataLoggers
                  , DataLogger
                  , open
                    -- * Simple queries
                  , getVersion
                  , getSampleCount
                  , getDeviceId
                  , DeviceId(..)
                    -- * Fetching samples
                  , Sample(..)
                  , SensorId(..)
                  , getSamples
                    -- * Settings
                  , Setting
                  , get
                  , set
                  , samplePeriod
                  , rtcTime
                  , acquiring
                  , acquireOnBoot
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

-- | Find possible data logger devices       
findDataLoggers :: IO [FilePath]
findDataLoggers = do
    let root = "/dev"                
    devices <- getDirectoryContents root
    return $ map (root </>) $ filter isACM devices
  where
    isACM = isSuffixOf "ttyACM" . reverse . dropWhile isDigit . reverse

data DataLogger = DataLogger { handle  :: Handle
                             , request :: TMVar (String, TMVar [String])
                             }

open :: MonadIO m => FilePath -> EitherT String m DataLogger
open device = do
    h <- liftIO $ hOpenSerial device defaultSerialSettings
    reply <- liftIO $ atomically newEmptyTMVar
    let dl = DataLogger h reply
    liftIO $ forkIO $ ioWorker dl
    -- Make sure things are working properly
    v <- getVersion dl
    return dl
    
ioWorker :: DataLogger -> IO ()
ioWorker (DataLogger h req) = forever $ do
    (cmd,replyVar) <- atomically $ takeTMVar req
    liftIO $ hPutStr h (cmd ++ "\n")
    reply <- go []
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
                             putTMVar req (cmd, reply)
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
    emptyTail []   = []
    emptyTail x:xs = xs

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

newtype DeviceId = DevId String    
                 deriving (Show, Eq, Ord)

getDeviceId :: MonadIO m => DataLogger -> EitherT String m DeviceId
getDeviceId dl = DevId <$> valueCommand dl "I" "device id"

newtype SensorId = SID Int
                 deriving (Show, Ord, Eq)

data Sample = Sample { sampleTime   :: Integer
                     , sampleSensor :: SensorId
                     , sampleValue  :: Float
                     }
            deriving (Show)

getSamples :: MonadIO m => DataLogger -> Int -> Int -> EitherT String m [Sample]
getSamples dl start count = do
    let cmd = intercalate " " ["g", show start, show count]
    reply <- command dl cmd
    let (errors, samples) = partitionEithers $ map parseSample reply
    when (not $ null errors) $ liftIO $ print errors
    return samples
  where
    parseSample :: String -> Either String Sample
    parseSample l =
      case words l of
        [time, sid, val]  ->
            Sample <$> readErr "Error parsing time" time
                   <*> fmap SID (readErr "Error parsing sensor ID" sid)
                   <*> readErr "Error parsing value" val
        _                 ->
            Left "getSamples: Not enough fields"

showBool :: Bool -> String
showBool True  = "1"
showBool False = "0"
         
readBool :: String -> Either String Bool
readBool "1" = Right True
readBool "0" = Right False
readBool  a  = Left ("Unexpected boolean value: "++a)
    
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
