{-# LANGUAGE Strict #-}

module New.Engine.Metric.WordPrefixBonus where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Match     as Match
import qualified New.Engine.Data.Substring as Substring

import Control.Lens ((?~))
import New.Engine.Metric (Metric (updateMetric, getMetric))
import New.Engine.Data.Score (Score (Score))
import New.Engine.Data.Substring (Substring)
import Data.Char (isLower, isUpper, isLetter)



-----------------------------
-- === WordPrefixBonus === --
-----------------------------

-- === Definition === --

data WordPrefixBonus = WordPrefixBonus
    { _multiplier       :: Int
    , _wordsPrefixes    :: Substring
    , _previousDataChar :: Maybe Char
    } deriving (Generic, Show)
makeLenses ''WordPrefixBonus


-- === API === --

startsNewWord :: Char -> Char -> Bool
startsNewWord c prevC = let xor a b = (a && not b) || (not a && b)
    in (isLetter prevC `xor` isLetter c) || (isLower prevC && isUpper c)


-- === Instances === --

instance Default WordPrefixBonus where def = WordPrefixBonus 6 def def

instance NFData  WordPrefixBonus

instance Metric  WordPrefixBonus where
    updateMetric dataChar charMatch updatedState = do
        bonusState <- State.get @WordPrefixBonus
        let prefixes     = bonusState ^. wordsPrefixes
            mayPrevChar  = bonusState ^. previousDataChar
            posInData    = updatedState ^. Match.positionInData
            revRange     = prefixes ^. Substring.reversedRange
            mayLastRange = head revRange
            mayRangeEnd  = view Substring.end <$> mayLastRange
            appendChar   = \end -> if posInData - 1 == end
                then Substring.addPosition end prefixes
                else prefixes
            appendWordHead = Substring.addPosition (posInData - 1) prefixes
            isWordHead     = case mayPrevChar of
                Nothing    -> True
                Just prevC -> startsNewWord dataChar prevC
            updatedPrefixes = if charMatch == Match.Equal then if isWordHead 
                        then appendWordHead
                        else maybe prefixes appendChar mayRangeEnd
                    else prefixes
            updatedBonusState = bonusState
                & wordsPrefixes    .~ updatedPrefixes
                & previousDataChar ?~ dataChar
        State.put @WordPrefixBonus updatedBonusState
    getMetric _ = do
        mult     <- State.use @WordPrefixBonus multiplier
        prefixes <- State.use @WordPrefixBonus wordsPrefixes
        let revRange        = prefixes ^. Substring.reversedRange
            accRangeLength  = \r -> let
                rLen = r ^. Substring.len
                in rLen * (rLen + 1) `quot` 2
            appendAccLength = \acc r -> acc + accRangeLength r
            points          = foldl appendAccLength def revRange
        pure $! Score $! mult * points
