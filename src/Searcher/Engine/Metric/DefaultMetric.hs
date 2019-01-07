{-# LANGUAGE Strict #-}

module Searcher.Engine.Metric.DefaultMetric where

import Prologue

import qualified Control.Lens as Lens
import qualified Searcher.Engine.Metric         as Metric

import Searcher.Engine.Data.Score             (Score)
import Searcher.Engine.Metric.MismatchPenalty (MismatchPenalty)
import Searcher.Engine.Metric.PrefixBonus     (PrefixBonus)
import Searcher.Engine.Metric.SequenceBonus   (SequenceBonus)
import Searcher.Engine.Metric.SuffixBonus     (SuffixBonus)
import Searcher.Engine.Metric.WordPrefixBonus (WordPrefixBonus)
import Searcher.Engine.Metric.WordSuffixBonus (WordSuffixBonus)



---------------------------
-- === DefaultMetric === --
---------------------------

-- === Definition === --

data DefaultMetric = DefaultMetric
    { _mismatchPenalty :: MismatchPenalty
    , _prefixBonus     :: PrefixBonus
    , _sequenceBonus   :: SequenceBonus
    , _suffixBonus     :: SuffixBonus
    , _wordPrefixBonus :: WordPrefixBonus
    , _wordSuffixBonus :: WordSuffixBonus
    } deriving (Generic, Show)
makeLenses      ''DefaultMetric
Lens.makePrisms ''DefaultMetric


-- === Instances === --

instance Default DefaultMetric where
    def = DefaultMetric def def def def def def

instance NFData DefaultMetric

instance Metric.State DefaultMetric where
    updateMetric metricState char matchKind matchState = metricState
        & mismatchPenalty %~ update
        & prefixBonus     %~ update
        & sequenceBonus   %~ update
        & suffixBonus     %~ update
        & wordPrefixBonus %~ update
        & wordSuffixBonus %~ update

        where update :: Metric.State a => a -> a
              update = \s -> Metric.updateMetric s char matchKind matchState

    getMetric metricState matchState = get (metricState ^. mismatchPenalty)
        + get (metricState ^. prefixBonus)
        + get (metricState ^. sequenceBonus)
        + get (metricState ^. suffixBonus)
        + get (metricState ^. wordPrefixBonus)
        + get (metricState ^. wordSuffixBonus)

        where get :: Metric.State a => a -> Score
              get = \s -> Metric.getMetric s matchState

