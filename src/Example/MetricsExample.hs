{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Example.MetricsExample where

import           Control.Algebra
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default.Class
import           Metrics


makeMetrics "SomeMetric"    ["ta", "tb", "tc", "td"]
makeMetrics "OtherMetic"    ["oa", "ob", "oc", "od"]
makeMetrics "SpecialMetric" ["sa", "sb", "sc", "sd"]
makeMetrics "OutMetric"     ["outCounter", "inCounter"]

foo
    :: ( Has
             (    Metric SomeMetric    :+: Metric OtherMetic
              :+: Metric SpecialMetric :+: Metric OutMetric
             )
             sig
             m
       , MonadIO m
       )
    => m ()
foo = do
    inc ta
    replicateM_ 10 $ do
        inc tb
        inc ob
        inc sa
        inc outCounter
    inc inCounter
    dec tb
    a  <- getVal ta
    b  <- getVal tb
    c  <- getVal tc
    d  <- getVal tc
    b' <- getVal ob
    liftIO $ print (a, b, c, d, b')

runFoo :: IO ()
runFoo =
    runMetric @SomeMetric
        $ runMetric @OtherMetic
        $ runMetric @SpecialMetric
        $ runMetric @OutMetric foo
