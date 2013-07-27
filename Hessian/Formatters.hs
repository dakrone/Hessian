-- Formatters for the different ES JSON formats
{-# LANGUAGE OverloadedStrings #-}

module Hessian.Formatters (formatStatus, formatHealth)
where

import Hessian.Data

formatStatus :: ESStatus -> String
formatStatus status = "Elasticsearch "
                      ++ (getNumber (getVersion status)) ++ " ["
                      ++ (getName status) ++ "], lucene: "
                      ++ (getLucene (getVersion status))


-- time     cluster       status nodes data pri shards relo init unassign
-- 11:56:14 elasticsearch green      1    1   0      0    0    0        0
formatHealth :: ESHealth -> String
formatHealth health = "<time> "
                      ++ (getClusterName health) ++ " "
                      ++ colorizeHealth (getStatusColor health) ++ " "
                      ++ (show (getNodes health)) ++ " "
                      ++ (show (getDataNodes health)) ++ " "
                      ++ (show (getPrimaryShards health)) ++ " "
                      ++ (show (getActiveShards health)) ++ " "
                      ++ (show (getRelocatingShards health)) ++ " "
                      ++ (show (getInitializingShards health)) ++ " "
                      ++ (show (getUnassignedShards health)) ++ " "

