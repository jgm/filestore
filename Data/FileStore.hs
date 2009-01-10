{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{- |
   Module      : Data.FileStore
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   Abstract interface to a versioned file store, which can be
   implemented using a revision-control system or database.
   Based on ideas from Sebastiaan Visser's "Network.Orchid.Core.Backend".
-}

module Data.FileStore
           ( module Data.FileStore.Types
           , module Data.FileStore.Git
           )
where

import Data.FileStore.Git
import Data.FileStore.Types

