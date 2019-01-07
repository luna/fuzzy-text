
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE Strict                  #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Searcher.Engine.Metric where

import Prologue

import qualified Data.TypeMap.Strict         as TypeMap
import qualified Searcher.Engine.Data.Match  as Match

import Data.TypeMap.Strict        (TypeMap)
import Searcher.Engine.Data.Score (Score)



-- TODO [Ara] some way to aggregate metrics


--------------------
-- === Metric === --
--------------------

-- === Definition === --

type MetricAggregate ts = TypeMap ts

class (Default a, NFData a) => Metric a where
    updateMetric :: a -> Char -> Match.CharMatch -> Match.State -> a
    getMetric :: a -> Match.State -> Score

type family Metrics ss :: Constraint where
    Metrics (s ': ss) = (Metric s, Metrics ss)
    Metrics '[]       = ()


-- === API === --

make :: forall ts . (Metrics ts, TypeMap.MakeDefault ts) => TypeMap ts
make = TypeMap.makeDefault @ts


-- === Update === --

class Metrics ts => Update (ts :: [Type]) where
    update :: TypeMap ts -> Char -> Match.CharMatch -> Match.State
        -> TypeMap ts

instance ( TypeMap.Prependable t ts, TypeMap.SplitHead t ts, Metric t
         , Metrics ts, Update ts )
    => Update ((t ': ts) :: [Type]) where
    update map char matchKind matchState = let
        (currentSt, mapRest) = TypeMap.splitHead map
        newCurrentState      = updateMetric currentSt char matchKind matchState
        updatedRest          = update mapRest char matchKind matchState
        in TypeMap.prepend newCurrentState updatedRest

instance Update ('[] :: [Type]) where
    update _ _ _ _ = TypeMap.empty


-- === Get === --

class Metrics ts => Get (ts :: [Type]) where
    get :: TypeMap ts -> Match.State -> Score

instance (TypeMap.SplitHead t ts, Metric t, Metrics ts, Get ts)
    => Get ((t ': ts) :: [Type]) where
    get map matchState = let
        (currentSt, mapRest) = TypeMap.splitHead map
        restScore            = get mapRest matchState
        currentScore         = getMetric currentSt matchState
        in currentScore + restScore

instance Get ('[] :: [Type]) where
    get _ _ = def @Score

