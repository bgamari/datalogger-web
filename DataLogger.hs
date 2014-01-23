module DataLogger ( findDataLoggers
                  , DataLogger
                  , open
                    -- * Simple queries
                  , getVersion
                  , getSampleCount
                  , getDeviceId
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
import Control.Monad (void, when)
import Control.Monad.IO.Class
import System.Hardware.Serialport
import System.IO       
import Data.Char (isSpace, isDigit)
import Data.List (isSuffixOf, intercalate)
import System.Directory (getDirectoryContents)       
import System.FilePath ((</>))       

-- | Find possible data logger devices       
findDataLoggers :: IO [FilePath]
findDataLoggers = do
    let root = "/dev"                
    devices <- getDirectoryContents root
    return $ map (root </>) $ filter isACM devices
  where
    isACM = isSuffixOf "ttyACM" . reverse . dropWhile isDigit . reverse

newtype DataLogger = DataLogger Handle

open :: FilePath -> EitherT String IO DataLogger
open device = do
    h <- liftIO $ hOpenSerial device defaultSerialSettings
    let dl = DataLogger h
    -- Make sure things are working properly
    v <- getVersion dl
    return dl
    
close :: DataLogger -> IO ()
close (DataLogger h) = hClose h      

writeCmd :: DataLogger -> String -> EitherT String IO ()
writeCmd (DataLogger h) cmd = do
    liftIO $ hPutStr h (cmd ++ "\n")

-- | Read a multi-line reply
readReply :: DataLogger -> EitherT String IO [String]
readReply (DataLogger h) = go []
  where
    go ls = do l <- strip `fmap` liftIO (hGetLine h)
               if l == ""
                 then return (reverse ls)
                 else go (l:ls)

-- | Read a single-line reply 
readSimpleReply :: DataLogger -> EitherT String IO String
readSimpleReply dl = do
    ls <- readReply dl
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
    b = tail $ dropWhile (/= c) xs

readReplyValues :: DataLogger -> EitherT String IO (M.Map String String)
readReplyValues dl = do
    M.fromList . catMaybes . map (fmap stripKeyValue . split '=') <$> readReply dl
  where
    stripKeyValue (a, b) = (strip a, strip b)

readReplyValue :: DataLogger -> String -> EitherT String IO String
readReplyValue dl key = do
    reply <- readReplyValues dl
    M.lookup key reply ?? ("Key "++key++" not found")

getVersion :: DataLogger -> EitherT String IO String
getVersion dl = do
    writeCmd dl "V"
    readReplyValue dl "version"

getSampleCount :: DataLogger -> EitherT String IO Int
getSampleCount dl = do
    writeCmd dl "n"
    read <$> readReplyValue dl "sample count"
    
getDeviceId :: DataLogger -> EitherT String IO String
getDeviceId dl = do
    writeCmd dl "I"
    readReplyValue dl "device id"

newtype SensorID = SID Int
                 deriving (Show, Ord, Eq)

data Sample = Sample { sampleTime   :: Integer
                     , sampleSensor :: SensorID
                     , sampleValue  :: Float
                     }
            deriving (Show)

getSamples :: DataLogger -> Int -> Int -> EitherT String IO [Sample]
getSamples dl start count = do
    writeCmd dl $ intercalate " " ["g", show start, show count]
    reply <- readReply dl
    let (samples, errors) = partitionEithers $ map parseSample reply
    when (not $ null errors)
      $ liftIO $ print errors
    return samples
  where
    parseSample l =
      case words l of
        [time, sid, val]  ->
            runEitherT $ 
            Sample <$> tryRead "Error parsing time" time
                   <*> fmap SID (tryRead "Error parsing sensor ID" sid)
                   <*> tryRead "Error parsing value" val

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
               
set :: DataLogger -> Setting a -> a -> EitherT String IO ()
set dl s value = do
    writeCmd dl (sCommand s++"="++sShow s value)
    void $ readReplyValue dl (sName s)
    
get :: DataLogger -> Setting a -> EitherT String IO a
get dl s = do
    writeCmd dl (sCommand s)
    reply <- readReplyValue dl (sName s)
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
