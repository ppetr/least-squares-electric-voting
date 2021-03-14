module Properties
    ( module Properties
    , module Types
    ) where

import Control.Monad (guard)
import Data.Foldable (maximumBy)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Monoid (Sum(..))
import Data.Ord (comparing)

import Types

-- | Returns the candidate preferred by the majority of voters, if there is one.
majorityWinner :: (Eq a, Hashable a, Integral n) => Prefs a n -> Maybe a
majorityWinner (Prefs ps) = do
  let top (_, []) = Nothing
      top (n, c : _) = Just (c, n)
  let ts = M.fromListWith (+) . catMaybes . map top  $ ps
  guard (not $ M.null ts)
  let (winner, count) = maximumBy (comparing snd) . M.toList $ ts
  guard (2 * count > getSum (foldMap Sum ts))
  return winner
