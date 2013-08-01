-- A thing for ES things
{-# LANGUAGE OverloadedStrings #-}

module Hessian (main)
where

-- Hessian imports
import Hessian.Data

-- Other imports
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as L
import Network
import Network.HTTP.Conduit
import qualified System.Posix.Env as E


httpGet :: String -> IO L.ByteString
httpGet url = withSocketsDo $ do
                request <- parseUrl url
                res <- withManager $ httpLbs request
                return (responseBody res)


main :: IO ()
main = do
  url <- E.getEnv "ESURL"
  json <- httpGet $ case url of
                      Nothing -> "http://localhost:9200/_cluster/health"
                      Just u -> u ++ "/_cluster/health"
  case (decode json :: Maybe ESHealth) of
    Nothing -> putStrLn "Unable to retrieve cluster health."
    Just health -> putStrLn $ show health
