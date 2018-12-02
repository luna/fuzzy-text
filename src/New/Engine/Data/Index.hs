{-# LANGUAGE Strict #-}
module New.Engine.Data.Index where

import Prologue hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Data.IntMap                 as IntMap

import Data.Map.Strict (Map)
import Data.IntMap (IntMap)


-------------------
-- === Index === --
-------------------


-- === Definition === --

newtype Index = Index Int deriving (Eq, Generic, Num, Ord, Show)
makeClassy ''Index


-- === API === --

isInvalid :: Index -> Bool
isInvalid = (< 0)

get :: State.Monad IndexMap m => m Index
get = do
    txtMap <- State.get @IndexMap
    let nextIndex = Index $! Map.size txtMap
    pure nextIndex
{-# INLINE get #-}


-- === Constants === --

notExists :: Index
notExists = -1


-- === Instances === --

instance NFData  Index



---------------------
-- === IndexMap === --
---------------------


-- === Definition === --

type IndexMap = Map Text Index

