-------------------------------------------------------------------------------
-- |
-- Metric
--
-- This module provides functionality for the creation of your own metrics for
-- use in the fuzzy-text searching algorithm.
--
-- A metric is a state type and accompanying set of functions (defined by the
-- `MetricState` typeclass) that allow for updating and retrieving the score
-- from a metric. Your state type should have an implementation of
-- `MetricState`, and this will allow the algorithm to use it internally via
-- some type-level magic.
--
-- The essence of a metric is that it computes a score based on the input text
-- and the text at the current node in the Trie.
--
-- This may not be clear, at first, so let's work through an example as follows.
-- We're going to create a metric `DummyMetric` that actually doesn't do
-- anything but return its default score at each call. It should be noted that
-- the module is intended to be imported qualified as `Metric`.
--
-- 1. Create your metric's state type:
--
--      ```
--      data DummyMetric = DummyMetric
--          { currentScore :: !Score
--          } deriving (Eq, Ord, Show)
--      ```
--
-- 2. Implement `MetricState` for your type:
--
--      ```
--      instance MetricState DummyMetric where
--          updateMetric st _ _ _ = st
--          getMetric st _ = st ^. currentScore
--      ```
--
-- 3. You'll note that Metric requires you to implement `Default` and `NFData`,
--    so let's do that:
--
--      ```
--      instance Default DummyMetric where
--          def = DummyMetric def
--
--      instance NFData DummyMetric
--      ```
--
-- 4. Now you have a metric, you can make a few more. Let's assume we also have
--    `DummyMetric2` and `DummyMetric3`, so we can make a type-level list of
--    these metrics. You'll need to have `-XDataKinds` enabled for this to work.
--
--      ```
--      type MyStates = '[DummyMetric, DummyMetric2, DummyMetric3]
--      ```
--
-- TODO [Ara] Make this match the real final interface.
--
-- 5. You can generate a default value of your metric by doing the following:
--
--      ```
--      defaultVal = Metric.make @MyStates
--      ```
--
--    This has type `defaultVal :: Metric.Metric MyStates`.
--
-- 6. Now you have this value it is as simple as passing it to your call to
--    `search` from the FuzzyText library.
--
-- This will then 'just work'. As the search is run, the update functions on
-- all of your metrics will be called, and used to score the results returned by
-- the search.
--
-- For examples of actually useful metrics, and further guidance on how to work
-- with this interface, please see the metrics included with the library in the
-- `Engine.Metric.*` modules.

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

-- TODO [Ara] Rename MetricStates -> States and MetricState -> State



--------------------
-- === Metric === --
--------------------

-- === Definition === --

type Metric ts = TypeMap ts

class (Default a, NFData a) => MetricState a where
    updateMetric :: a -> Char -> Match.CharMatch -> Match.State -> a
    getMetric :: a -> Match.State -> Score

type family MetricStates ss :: Constraint where
    MetricStates (s ': ss) = (MetricState s, MetricStates ss)
    MetricStates '[]       = ()


-- === API === --

make :: forall ts . (MetricStates ts, TypeMap.MakeDefault ts) => TypeMap ts
make = TypeMap.makeDefault @ts


-- === Update === --

class MetricStates ts => Update (ts :: [Type]) where
    update :: Metric ts -> Char -> Match.CharMatch -> Match.State -> Metric ts

instance ( TypeMap.Prependable t ts, TypeMap.SplitHead t ts, MetricState t
         , MetricStates ts, Update ts )
    => Update ((t ': ts) :: [Type]) where
    update map char matchKind matchState = let
        (currentSt, mapRest) = TypeMap.splitHead map
        newCurrentState      = updateMetric currentSt char matchKind matchState
        updatedRest          = update mapRest char matchKind matchState
        in TypeMap.prepend newCurrentState updatedRest

instance Update ('[] :: [Type]) where
    update _ _ _ _ = TypeMap.empty


-- === Get === --

class MetricStates ts => Get (ts :: [Type]) where
    get :: Metric ts -> Match.State -> Score

instance (TypeMap.SplitHead t ts, MetricState t, MetricStates ts, Get ts)
    => Get ((t ': ts) :: [Type]) where
    get map matchState = let
        (currentSt, mapRest) = TypeMap.splitHead map
        restScore            = get mapRest matchState
        currentScore         = getMetric currentSt matchState
        in currentScore + restScore

instance Get ('[] :: [Type]) where
    get _ _ = def @Score


-- === Edit === --
type Edit ts = (Update ts, Get ts)

