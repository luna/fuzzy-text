{-# LANGUAGE Strict #-}

module Searcher.Engine.Metric.SequenceBonus where

import Prologue

import qualified Control.Monad.State.Layered    as State
import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Substring as Substring

import Searcher.Engine.Data.Score (Score (Score))
import Searcher.Engine.Metric     (Metric (getMetric, updateMetric))



---------------------------
-- === SequenceBonus === --
---------------------------

-- === Definition === --

data SequenceBonus = SequenceBonus
    { _multiplier :: Int
    } deriving (Generic, Show)
makeLenses ''SequenceBonus


-- === Instances === --

instance Default SequenceBonus where def = SequenceBonus 10

instance NFData  SequenceBonus

instance Metric  SequenceBonus where
    updateMetric _ _ _   = pure ()

    getMetric matchState = pure 0 -- let
        -- revRange
            -- = matchState ^. Match.currentSubstring . Substring.reversedRange
        -- getRangeScore = \r -> let
            -- rLen = r ^. Substring.len
            -- rLenPred = rLen - 1
            -- points   = rLen * rLenPred `quot` 2
            -- in max 0 points
        -- points      = foldl (\acc r -> acc + getRangeScore r) def revRange
        -- mkScore     = \m -> Score $! m * points
        -- multM       = State.use @SequenceBonus multiplier
        -- in mkScore <$> multM

