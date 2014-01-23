{-# LANGUAGE OverloadedStrings #-}
                
import Web.Scotty                
import DataLogger

main = scotty 3000 $ do
    get "/hello-world" $ do
        html "<h1>Hello World</h1>"
