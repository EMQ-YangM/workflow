{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Metrics
    ( Metric(..)
    , addOne
    , subOne
    , getVal
    , putVal
    , runMetric
    , runMetricWith
    , creatVec
    , makeMetrics
    , Vec(..)
    , K(..)
    , Vlength(..)
    ) where

import           Control.Carrier.Reader         ( Algebra
                                                , Has
                                                , ReaderC(..)
                                                , runReader
                                                )
import           Control.Effect.Labelled        ( type (:+:)(..)
                                                , Algebra(..)
                                                , Has
                                                , send
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Data                      ( Proxy(..) )
import           Data.Kind                      ( Type )
import           Data.Maybe                     ( Maybe(Nothing)
                                                , fromJust
                                                , fromMaybe
                                                )
import           Data.Vector.Mutable            ( IOVector
                                                , replicate
                                                , unsafeModify
                                                , unsafeRead
                                                , unsafeWrite
                                                )
import           GHC.TypeLits                   ( KnownSymbol
                                                , Symbol
                                                , symbolVal
                                                )
import           Prelude                 hiding ( replicate )
import           Text.Read                      ( readMaybe )

import           Data.Default.Class             ( Default(..) )
import           Language.Haskell.TH     hiding ( Type )
import qualified Prelude                       as P

type K :: Symbol  -> Type
data K s where
    K ::K s

toi :: forall s . (KnownSymbol s) => K s -> Int
toi _ = fromJust $ readMaybe $ symbolVal (Proxy :: Proxy s)

get :: (KnownSymbol s, Default a) => (a -> K s) -> Int
get v1 = toi . v1 $ def

class Vlength a where
    vlength :: a -> Int

fun
    :: (KnownSymbol s, Default a)
    => IOVector Int
    -> (a -> K s)
    -> (Int -> Int)
    -> IO ()
fun v idx f = unsafeModify v f (get idx)

gv :: (KnownSymbol s, Default a) => IOVector Int -> (a -> K s) -> IO Int
gv v idx = unsafeRead v (get idx)

pv :: (KnownSymbol s, Default a) => IOVector Int -> (a -> K s) -> Int -> IO ()
pv v idx = unsafeWrite v (get idx)

addOne1 :: (KnownSymbol s, Default a) => IOVector Int -> (a -> K s) -> IO ()
addOne1 v idx = fun v idx (+ 1)

subOne1 :: (KnownSymbol s, Default a) => IOVector Int -> (a -> K s) -> IO ()
subOne1 v idx = fun v idx (\x -> x - 1)

type Metric :: Type -> (Type -> Type) -> Type -> Type
data Metric v m a where
    AddOne ::KnownSymbol s => (v -> K s) -> Metric v m ()
    SubOne ::KnownSymbol s => (v -> K s) -> Metric v m ()
    GetVal ::KnownSymbol s => (v -> K s) -> Metric v m Int
    PutVal ::KnownSymbol s => (v -> K s) -> Int -> Metric v m ()

addOne :: (Has (Metric v) sig m, KnownSymbol s) => (v -> K s) -> m ()
addOne g = send (AddOne g)

subOne :: (Has (Metric v) sig m, KnownSymbol s) => (v -> K s) -> m ()
subOne g = send (SubOne g)

getVal :: (Has (Metric v) sig m, KnownSymbol s) => (v -> K s) -> m Int
getVal g = send (GetVal g)

putVal :: (Has (Metric v) sig m, KnownSymbol s) => (v -> K s) -> Int -> m ()
putVal g v = send (PutVal g v)

newtype MetriC v m a= MetriC { unMetric :: ReaderC (IOVector Int) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m, Default v) => Algebra (Metric v :+: sig ) (MetriC v m) where
    alg hdl sig ctx = MetriC $ ReaderC $ \iov -> case sig of
        L (AddOne g) -> do
            liftIO $ addOne1 iov g
            pure ctx
        L (SubOne g) -> do
            liftIO $ subOne1 iov g
            pure ctx
        L (GetVal g) -> do
            v <- liftIO $ gv iov g
            pure (v <$ ctx)
        L (PutVal g v) -> do
            liftIO $ pv iov g v
            pure ctx
        R signa -> alg (runReader iov . unMetric . hdl) signa ctx

runMetric
    :: forall v m a . (MonadIO m, Default v, Vlength v) => MetriC v m a -> m a
runMetric f = do
    v <- liftIO creatVec
    runMetricWith v f

data Vec v = Vec v (IOVector Int)

creatVec :: forall v . (Vlength v, Default v) => IO (Vec v)
creatVec = do
    iov <- replicate (vlength @v undefined) 0
    pure (Vec def iov)

runMetricWith :: forall v m a . (MonadIO m) => Vec v -> MetriC v m a -> m a
runMetricWith (Vec v iov) f = runReader iov $ unMetric f

makeMetrics :: String -> [String] -> Q [Dec]
makeMetrics bn ls = do
    classTypeDef <- fromMaybe (error "you need impore Data.Default.Class ")
        <$> lookupTypeName "Default"
    classTypeLen <- fromJust <$> lookupTypeName "Vlength"

    let contTypeV = mkName bn
    methodDef <- fromMaybe (error "you need impore Data.Default.Class ")
        <$> lookupValueName "def"
    methodVlen <- fromJust <$> lookupValueName "vlength"

    let vVal = mkName bn
    kVal <- fromJust <$> lookupValueName "K"
    let aal = P.foldl (\acc var -> AppE acc (ConE var))
                      (ConE vVal)
                      (P.replicate (Prelude.length ls) kVal)
    let iDec = InstanceD Nothing
                         []
                         (AppT (ConT classTypeDef) (ConT contTypeV))
                         [mDec]
        mDec  = ValD (VarP methodDef) (NormalB aal) []

        iDec1 = InstanceD Nothing
                          []
                          (AppT (ConT classTypeLen) (ConT contTypeV))
                          [iFun]
        iFun = FunD
            methodVlen
            [ Clause
                  [WildP]
                  (NormalB (LitE (IntegerL $ fromIntegral $ P.length ls)))
                  []
            ]

    kType <- fromJust <$> lookupTypeName "K"
    let ddd  = DataD [] (mkName bn) [] Nothing [cons] []
        cons = RecC
            (mkName bn)
            [ ( mkName b
              , Bang NoSourceUnpackedness NoSourceStrictness
              , AppT (ConT kType) (LitT (StrTyLit (show a)))
              )
            | (a, b) <- zip [0, 1 ..] ls
            ]
    pure [ddd, iDec, iDec1]
