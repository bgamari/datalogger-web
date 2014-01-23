{-# LANGUAGE OverloadedStrings #-}
                
import Web.Scotty                
import qualified DataLogger as DL
import Control.Error
import Control.Monad.IO.Class

       {-
main = scotty 3000 $ do
    get "/hello-world" $ do
        html "<h1>Hello World</h1>"

    -}
    
main = runEitherT $ do
    dl <- DL.open "/dev/ttyACM0"
    DL.getVersion dl >>= liftIO . print
