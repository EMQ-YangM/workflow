{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Example.Example1.Servers where
import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.IntMap                   as IntMap
import           Data.Map
import qualified Data.Map                      as Map
import           Data.Proxy
import           Example.Example1.Type
import           HasServer
import           HasWorkGroup
import           Metric
import           System.Random
import           Type
import           Util

handCommand
    :: ( HasServer "log" SigLog '[Log] sig m
       , Has (Error Stop :+: Reader Name) sig m
       , MonadIO m
       )
    => SigCommand f
    -> m ()
handCommand = \case
    SigCommand1 (Finish tmv) -> do
        name <- ask @Name
        resp tmv ()
        liftIO $ putStrLn $ name ++ " server stop"
        throwError Stop
    SigCommand2 (Talk s) -> do
        name <- ask @Name
        l3 $ "talk " ++ s

-- db server

dbServer
    :: ( Has (MessageChan SigDB :+: MessageChan SigCommand) sig m
       ------------------------
       , HasServer "log" SigLog '[Log] sig m
       , HasServer "auth" SigAuth '[VerifyToken] sig m
       ------------------------
       , Has
             (   State (Map Int String)
             :+: Metric DBmetric
             :+: Error Stop
             :+: Reader Name
             )
             sig
             m
       , MonadIO m
       )
    => m ()
dbServer = forever $ withTwoMessageChan @SigCommand @SigDB
    (\case
        SigCommand1 (Finish tmv) -> do
            name <- ask @Name
            m    <- get @(Map Int String)
            put @(Map Int String) Map.empty
            liftIO $ putStrLn $ name ++ " server stop"
            resp tmv ()
            throwError Stop
        SigCommand2 (Talk s) -> do
            name <- ask @Name
            l3 $ "talk " ++ s
    )
    (\case
        SigDB1 (WriteUser token k v) -> do
            vt <- call @"auth" $ VerifyToken token
            if vt
                then do
                    inc db_write
                    modify (Map.insert k v)
                    l1 "write DB successed"
                else l4 "write DB filed"
        SigDB2 (GetAllMetric tmv) -> do
            am <- getAll @DBmetric Proxy
            resp tmv am
        SigDB3 (GetUser k tmv) -> do
            inc db_read
            val <- gets (Map.lookup k)
            resp tmv val
            l1 "read DB"
        SigDB4 (GetDBSize tmv) -> do
            v <- gets @(Map Int String) Map.size
            resp tmv v
        SigDB5 (GetAllUser tmv) -> do
            v <- gets @(Map Int String) Map.toList
            resp tmv v
        SigDB6 (DeleteAll token tmv) -> do
            vt <- call @"auth" $ VerifyToken token
            if vt
                then do
                    v <- gets @(Map Int String) Map.size
                    put @(Map Int String) Map.empty
                    resp tmv v
                    l4 "delete all user"
                else do
                    resp tmv 0
                    l4 "verify token faild, you can't delete db"
    )

--- log server 

logServer
    :: ( Has (MessageChan SigLog :+: MessageChan SigCommand) sig m
       ------------------------
       , HasServer "log" SigLog '[Log] sig m
       , HasServer "auth" SigAuth '[VerifyToken] sig m

       , Has (    Metric LogMetric
              :+: Error Stop
              :+: Reader Name
              :+: State Level
            )
            sig
            m
        , MonadIO m)
    => m ()
logServer = do
    forever $ withTwoMessageChan @SigCommand @SigLog handCommand $ \case
        SigLog1 l@(Log lv from s) -> do
            lv' <- get @Level
            if lv' <= lv
                then do
                    v <- getVal log_total
                    inc log_total
                    liftIO $ putStrLn $ show v ++ ": " ++ show l
                else pure ()
        SigLog2 (Allmetric tmv) -> do
            inc log_t
            am <- getAll @LogMetric Proxy
            resp tmv am
        SigLog3 _                       -> undefined
        SigLog4 (SetLevel token lv tmv) -> do
            t <- call @"auth" $ VerifyToken token
            if t
                then put lv >> liftIO (print "set level success")
                else liftIO (print "verifyToken faild, set level faild")
            resp tmv ()

---- add server

addServer
    :: ( HasServer "log" SigLog '[Log] sig m
       , Has (MessageChan SigAdd
             :+: Metric AddMetric
             :+: MessageChan SigCommand
             :+: Error Stop
             :+: Reader Name
             ) sig m
       , MonadIO m
       )
    => m ()
addServer =
    forever $ withTwoMessageChan @SigCommand @SigAdd handCommand $ \case
        SigAdd1 Add1 -> do
            inc add_total
            l1 "add1"
        SigAdd2 Sub1 -> do
            dec add_total
            l1 "sub1"
        SigAdd3 (GetAllMetric tmv) -> do
            am <- getAll @AddMetric Proxy
            resp tmv am

-- Auth server
authServer
    :: ( HasServer "log" SigLog '[Log] sig m
       , Has (MessageChan SigAuth
            :+: State [String]
            :+: MessageChan SigCommand
            :+: Error Stop
            :+: Reader Name
            ) sig m
       , MonadIO m)
    => m ()
authServer =
    forever $ withTwoMessageChan @SigCommand @SigAuth handCommand $ \case
        SigAuth1 (GetToken tmv) -> do
            rs <- replicateM 4 $ randomRIO ('a', 'z')
            modify (rs :)
            l4 $ "create token: " ++ rs
            resp tmv rs
        SigAuth2 (VerifyToken s tmv) -> do
            v <- gets @[String] (s `elem`)
            l4 $ "verify token [" ++ s ++ "] " ++ show v
            resp tmv v
        SigAuth3 (GetAllTokens tmv) -> do
            s <- get @[String]
            resp tmv s
        SigAuth4 (DeleteAllTokens token tmv) -> do
            if token == "admin"
                then
                    put @[String] []
                    >> l4 ("delete all tokens by " ++ token)
                    >> resp tmv True
                else
                    l4 ("delete all tokens failed by" ++ token)
                        >> resp tmv False

lFun
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => Level
    -> String
    -> m ()
lFun l s = do
    name <- ask @Name
    cast @"log" (Log l name s)

l1
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => String
    -> m ()
l1 = lFun L1
l2
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => String
    -> m ()
l2 = lFun L2
l3
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => String
    -> m ()
l3 = lFun L3
l4
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => String
    -> m ()
l4 = lFun L4
