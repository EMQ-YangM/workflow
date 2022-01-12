{-# LANGUAGE GADTs, TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HasServerTH where

import           Control.Concurrent
import           Data.Maybe
import           HasServer
import           Language.Haskell.TH

mkSigAndClass :: String -> [Name] -> Q [Dec]
mkSigAndClass sname gs = do
    sig <- mkSig sname gs
    cls <- mkClass sname gs
    pure $ sig ++ cls


mkSig :: String -> [Name] -> Q [Dec]
mkSig sname gs = do
    let t1  = mkName sname
        dec = DataD
            []
            t1
            [PlainTV (mkName "a")]
            Nothing
            [ GadtC
                  [mkName (sname ++ show idx)]
                  [(Bang NoSourceUnpackedness NoSourceStrictness, ConT g1)]
                  (AppT (ConT t1) (ConT g1))
            | (idx, g1) <- zip [1 ..] gs
            ]
            []
    pure [dec]

mkClass :: String -> [Name] -> Q [Dec]
mkClass sname gs = do
    tosig  <- fromMaybe (error "not find 1 ToSig") <$> lookupTypeName "ToSig"
    method <- fromMaybe (error "not find 2 toSig") <$> lookupValueName "toSig"
    let
        decs =
            [ InstanceD
                  Nothing
                  []
                  (AppT (AppT (ConT tosig) (ConT g1)) (ConT (mkName sname)))
                  [ FunD
                        method
                        [ Clause
                              [VarP $ mkName "ms"]
                              (NormalB
                                  (AppE (ConE (mkName (sname ++ show idx)))
                                        (VarE (mkName "ms"))
                                  )
                              )
                              []
                        ]
                  ]
            | (idx, g1) <- zip [1 ..] gs
            ]
    pure decs