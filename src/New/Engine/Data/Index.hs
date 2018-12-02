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

type Index = Int


-- === API === --

isInvalid :: Index -> Bool
isInvalid = (< 0)

get :: State.Monad IndexMap m => m Index
get = State.get @IndexMap >>= pure . Map.size
{-# INLINE get #-}


-- === Constants === --

notExists :: Index
notExists = -1



---------------------
-- === IndexMap === --
---------------------

-- === Definition === --

type IndexMap = Map Text Index

