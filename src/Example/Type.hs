{-# LANGUAGE GADTs, TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Example.Type where

import           Control.Concurrent
import           Metric
import           TH
import           Type

data Message1  where
     Message1 ::String -> MVar String -> Message1
    --  Message1 ::String -> MVar String %1 -> Message1
    --                   add linear type  (call function, server must response)
data Stop = Stop
newtype GetAllMetric = GetAllMetric (MVar [Int])

data WriteUser = WriteUser Int String
data GetUser = GetUser Int (MVar (Maybe String))
newtype GetDBSize = GetDBSize (MVar Int)
newtype GetAllUser = GetAllUser (MVar [Int])
newtype DeleteAll = DeleteAll (MVar Int)

data LogMessage = LogMessage String String

mkSigAndClass "SigMessage"
  [ ''Message1
  , ''GetAllMetric
  ]

mkSigAndClass "SigDB"
  [ ''WriteUser
  , ''GetAllMetric
  , ''GetUser
  , ''GetDBSize
  , ''GetAllUser
  , ''DeleteAll
  ]

mkSigAndClass "SigLog"
  [ ''LogMessage
  , ''GetAllMetric
  ]

type Token = String

data GetToken = GetToken Int (MVar String)
data VerifyToken = VerifyToken String (MVar Bool)

mkSigAndClass "SigAuth"
  [ ''GetToken
  , ''VerifyToken
  ]

-- Auth
-- getToken
-- verifyToken

data Add1 = Add1
data Sub1 = Sub1

mkSigAndClass "SigAdd" [''Add1, ''Sub1, ''GetAllMetric]

mkMetric "ClientMetric" ["total_loop", "t_m1", "t_m2", "t_str"]

mkMetric "DBmetric" ["db_write", "db_read"]

mkMetric "LogMetric" ["log_total", "log_t"]

mkMetric "AddMetric" ["add_total"]

type Name = String

data Level = L1 | L2 | L3 | L4

data Log = Log Level String
newtype Allmetric = Allmetric (MVar [Int])

instance Show Level where
  show = \case
    L1 -> "😎"
    L2 -> "🥶"
    L3 -> "👿"
    L4 -> "👾"

instance Show Log where
  show (Log l s) = show l ++ " " ++ s

mkSigAndClass "SigLog1"
    [ ''Log
    , ''Allmetric
    ]

mkMetric "LogMetric1" ["log_all"]

newtype WorkInfo = WorkInfo (MVar (String, Int))
newtype AllCycle = AllCycle (MVar (Int, Int))

mkSigAndClass "SigCom"
    [ ''Stop
    , ''WorkInfo
    , ''AllCycle
    ]

mkMetric "WorkMetric" ["w_total"]
