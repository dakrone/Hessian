-- Types for the different ES JSON formats
{-# LANGUAGE OverloadedStrings #-}

module Hessian.Data (ESVersion,
                     ESStatus,
                     ESHealth,
                     colorizeHealth,
                     getNumber,
                     getSnapshot,
                     getLucene,
                     getName,
                     getOk,
                     getStatus,
                     getTagline,
                     getVersion,
                     getPrimaryShards,
                     getActiveShards,
                     getClusterName,
                     getInitializingShards,
                     getDataNodes,
                     getNodes,
                     getRelocatingShards,
                     getStatusColor,
                     getUnassignedShards)
where

import Data.Aeson ((.:), FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
-- for colorization
import Text.PrettyPrint.ANSI.Leijen (green, yellow, red)


-- Version string
data ESVersion = ESVersion { getNumber   :: String
                           , getSnapshot :: Bool
                           , getLucene   :: String
                           } deriving (Show)

instance FromJSON ESVersion where
    parseJSON (Object v) =
        ESVersion <$> (v .: "number")         <*>
                      (v .: "snapshot_build") <*>
                      (v .: "lucene_version")


-- Basic info about ES cluster
data ESStatus = ESStatus { getName     :: String
                         , getOk       :: Bool
                         , getStatus   :: Integer
                         , getTagline  :: String
                         , getVersion  :: ESVersion
                         } deriving (Show)

instance FromJSON ESStatus where
    parseJSON (Object v) =
        ESStatus <$> (v .: "name")    <*>
                     (v .: "ok")      <*>
                     (v .: "status")  <*>
                     (v .: "tagline") <*>
                     (v .: "version")

-- Health information from _cluster/health
data ESHealth = ESHealth { getPrimaryShards      :: Integer,
                           getActiveShards       :: Integer,
                           getClusterName        :: String,
                           getInitializingShards :: Integer,
                           getDataNodes          :: Integer,
                           getNodes              :: Integer,
                           getRelocatingShards   :: Integer,
                           getStatusColor        :: String,
                           getUnassignedShards   :: Integer
                         }

instance FromJSON ESHealth where
    parseJSON (Object v) =
        ESHealth <$> (v .: "active_primary_shards") <*>
                     (v .: "active_shards") <*>
                     (v .: "cluster_name") <*>
                     (v .: "initializing_shards") <*>
                     (v .: "number_of_data_nodes") <*>
                     (v .: "number_of_nodes") <*>
                     (v .: "relocating_shards") <*>
                     (v .: "status") <*>
                     (v .: "unassigned_shards")

-- take a color like "yellow" and return it with colored escape codes
colorizeHealth :: String -> String
colorizeHealth "green"  = show $ green "green"
colorizeHealth "yellow" = show $ yellow "yellow"
colorizeHealth "red"    = show $ red "red"
colorizeHealth _        = "Error"


instance Show ESHealth where
    show health = "<time> "
                  ++ (getClusterName health) ++ " "
                  ++ colorizeHealth (getStatusColor health) ++ " "
                  ++ (show (getNodes health)) ++ " "
                  ++ (show (getDataNodes health)) ++ " "
                  ++ (show (getPrimaryShards health)) ++ " "
                  ++ (show (getActiveShards health)) ++ " "
                  ++ (show (getRelocatingShards health)) ++ " "
                  ++ (show (getInitializingShards health)) ++ " "
                  ++ (show (getUnassignedShards health))
