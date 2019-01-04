
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Searcher.Engine.Metric where

import Prologue

import qualified Data.TypeMap.Strict         as TypeMap
import qualified Searcher.Engine.Data.Match  as Match

import Data.TypeMap.Strict        (TypeMap)
import Searcher.Engine.Data.Score (Score)



--------------------
-- === Metric === --
--------------------

-- === Definition === --

class (Default a, NFData a) => Metric a where
    updateMetric :: a -> Char -> Match.CharMatch -> Match.State -> a
    getMetric :: a -> Match.State -> Score

type family Metrics ss :: Constraint where
    Metrics (s ': ss) = (Metric s, Metrics ss)
    Metrics '[]       = ()


-- === API === --

genMetrics :: forall ts . TypeMap.MakeDefault ts => TypeMap ts
genMetrics = TypeMap.makeDefault @ts


-- === UpdateAll === --

class UpdateAll (ts :: [Type]) where
    updateAll :: TypeMap ts -> Char -> Match.CharMatch -> Match.State
        -> TypeMap ts

instance ( TypeMap.Prependable t ts, TypeMap.SplitHead t ts, Metric t
         , Metrics ts, UpdateAll ts )
    => UpdateAll ((t ': ts) :: [Type]) where
    updateAll map char matchKind matchState = let
        (currentSt, mapRest) = TypeMap.splitHead map
        newCurrentState      = updateMetric currentSt char matchKind matchState
        updatedRest          = updateAll mapRest char matchKind matchState
        in TypeMap.prepend newCurrentState updatedRest

instance UpdateAll ('[] :: [Type]) where
    updateAll map _ _ _ = map


-- === GetAll === --

class GetAll (ts :: [Type]) where
    getAll :: TypeMap ts -> Match.State -> Score

instance (TypeMap.SplitHead t ts, Metric t, Metrics ts, GetAll ts)
    => GetAll ((t ': ts) :: [Type]) where
    getAll map matchState = let
        (currentSt, mapRest) = TypeMap.splitHead map
        restScore            = getAll mapRest matchState
        currentScore         = getMetric currentSt matchState
        in currentScore + restScore

instance GetAll ('[] :: [Type]) where
    getAll _ _ = def @Score

