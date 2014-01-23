module DataLogger ( open
                  , getVersion
                  , getSampleCount
                  , getDeviceId
                  ) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))
import Control.Error
import Control.Monad.IO.Class
import System.Hardware.Serialport
import System.IO       
import Data.Char (isSpace)

newtype DataLogger = DataLogger Handle

open :: FilePath -> EitherT String IO DataLogger
open device = do
    h <- liftIO $ hOpenSerial device defaultSerialSettings
    let dl = DataLogger h
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
    writeCmd dl "v"
    readReplyValue dl "version"

getSampleCount :: DataLogger -> EitherT String IO Int
getSampleCount dl = do
    writeCmd dl "n"
    read <$> readReplyValue dl "sample count"
    
getDeviceId :: DataLogger -> EitherT String IO String
getDeviceId dl = do
    writeCmd dl "I"
    readReplyValue dl "device id"

