{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Productivity
import Control.Auto
import Prelude hiding          ((.), id)

import Network.Wai
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp (run)

import Data.Aeson

app :: Application
app request respond = do
    let command = parseRequest (pathInfo request)
    (prod,_) <- stepAuto fullApp command
    let prod' = prod
    respond $ responseLBS
        status200
        [("Content-Type", "text/json"), ("Access-Control-Allow-Origin", "*")]
        (encode prod')

main :: IO ()
main = do
    Warp.run 8080 Main.app
