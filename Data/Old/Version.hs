{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable #-}
#else
{-# LANGUAGE DeriveDataTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Old.Version
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE in the ghc repo)
--
-- Maintainer  :  Chris Blake <chris@code.sc>
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification in ReadP)
--
-- A general library for representation and manipulation of versions.
--
-- Versioning schemes are many and varied, so the version
-- representation provided by this library is intended to be a
-- compromise between complete generality, where almost no common
-- functionality could reasonably be provided, and fixing a particular
-- versioning scheme, which would probably be too restrictive.
--
-- So the approach taken here is to provide a representation which
-- subsumes many of the versioning schemes commonly in use, and we
-- provide implementations of 'Eq', 'Ord', 'Read', and 'Show',
-- which will be appropriate for some applications, but not all.
--
-- This version of the module is forked from base, preserving the
-- 'versionTags' field of the 'Version' type that will be removed in GHC 7.12
-- (cf. <https://ghc.haskell.org/trac/ghc/ticket/2496>).
--
-- This version not only preserves 'versionTags', but also eliminates the
-- showing and parsing functions in favor of sensible, hand-derived 'Show'
-- and 'Read' instances, such that:
--
-- @show (read version) == id version@
--
-- For compatibility with base, conversion functions to and from
-- 'Data.Version.Version' are provided.
-----------------------------------------------------------------------------

module Data.Old.Version (
        -- * The @Version@ type
        Version(..)
        -- * Conversions from/to 'Data.Version.Version'
      , toBase
      , fromBase
  ) where

import Control.Monad                   ( Monad(..), liftM )
import Data.Char                       ( isDigit, isAlphaNum )
import Data.Eq
import Data.Int                        ( Int )
import Data.List
import Data.Ord
import Data.String                     ( IsString(..), String )
import Data.Typeable                   ( Typeable )
import qualified Data.Version as V     ( Version(..) )
import GHC.Read
import GHC.Show
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec ( lift )
import Text.Read                       ( read )

{- |
A 'Version' represents the version of a software entity.

An instance of 'Eq' is provided, which implements exact equality
by first comparing the 'versionBranch' fields, followed by comparing
the 'versionTags' fields.

An instance of 'Ord' is also provided, which gives lexicographic
ordering on the 'versionBranch' fields (i.e. 2.1 > 2.0, 1.2.3 > 1.2.2,
etc.).  This is expected to be sufficient for many uses, but note that
you may need to use a more specific ordering for your versioning
scheme.  For example, some versioning schemes may include pre-releases
which have tags @\"pre1\"@, @\"pre2\"@, and so on, and these would need to
be taken into account when determining ordering.  In some cases, date
ordering may be more appropriate, so the application would have to
look for @date@ tags in the 'versionTags' field and compare those.
The bottom line is, don't always assume that 'compare' and other 'Ord'
operations are the right thing for every 'Version'.

Similarly, concrete representations of versions may differ.  One
possible concrete representation is provided by the 'Show' and 'Read'
instances, but depending on the application a different concrete
representation may be more appropriate.
-}
data Version =
  Version { versionBranch :: [Int],
                -- ^ The numeric branch for this version.  This reflects the
                -- fact that most software versions are tree-structured; there
                -- is a main trunk which is tagged with versions at various
                -- points (1,2,3...), and the first branch off the trunk after
                -- version 3 is 3.1, the second branch off the trunk after
                -- version 3 is 3.2, and so on.  The tree can be branched
                -- arbitrarily, just by adding more digits.
                --
                -- We represent the branch as a list of 'Int', so
                -- version 3.2.1 becomes [3,2,1].  Lexicographic ordering
                -- (i.e. the default instance of 'Ord' for @[Int]@) gives
                -- the natural ordering of branches.

           versionTags :: [String]
                -- ^ A version can be tagged with an arbitrary list of strings.
                -- The interpretation of the list of tags is entirely dependent
                -- on the entity that this version applies to.
        }
  deriving (Eq, Typeable)

instance Ord Version where
  v1 `compare` v2 = versionBranch v1 `compare` versionBranch v2

-- -----------------------------------------------------------------------------
-- A concrete representation of 'Version'

-- | Provides one possible concrete representation for 'Version'.  For
-- a version with 'versionBranch' @= [1,2,3]@ and 'versionTags'
-- @= [\"tag1\",\"tag2\"]@, the output will be @1.2.3-tag1-tag2@.
--
instance Show Version where
  show (Version branch tags) = concat (intersperse "." (map show branch)) ++
     concatMap ('-':) tags

instance Read Version where
  readPrec = lift ( do branch <- sepBy1 (liftM read (munch1 isDigit)) (char '.')
                       tags   <- many (char '-' >> munch1 isAlphaNum)
                       return Version{versionBranch=branch, versionTags=tags}
                  )

instance IsString Version where
    fromString = read

-- | Convert to a 'Data.Version.Version', stripping off the 'versionTags'.
toBase :: Version -> V.Version
toBase (Version branch' _) = V.Version branch' []

-- | Convert from a 'Data.Version.Version', adding an empty set of 'versionTags'.
fromBase :: V.Version -> Version
fromBase (V.Version branch' _) = Version branch' []
