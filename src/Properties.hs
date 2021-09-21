-- Copyright 2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Properties
    ( module Properties
    , module Types
    ) where

import Control.Monad (guard)
import qualified Data.Graph as G
import Data.Foldable (maximumBy, toList)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import qualified Data.Set as S

import Types

-- | Pairs each candidate with a set of other candidates beaten or equal in a
-- head-to-head election.
orderedPairs :: (Ord a, Num n, Ord n) => Prefs a n -> [(a, [a])]
orderedPairs (Prefs ps) =
    M.toList . M.map (M.keys . M.filter (>= 0))
    . M.fromListWith (M.unionWith (+))
    . concatMap (uncurry pairs) $ ps
  where
    pairs _ [] = []
    pairs n (x : xs)
        = concat [[(x, M.singleton y n),
                   (y, M.singleton x (-n))] | y <- xs] ++ pairs n xs

-- | Computes the Smith set of a given set of preferences.
-- See https://en.wikipedia.org/wiki/Smith_set
smithSet :: (Ord a, Num n, Ord n) => Prefs a n -> [a]
smithSet ps
    | null (G.vertices g) = toList . foldMap (S.fromList . snd) . getPrefs $ ps
    | otherwise         = [x | v <- toList . last $ components
                             , let (x, _, _) = vf v]
  where
    (g, vf, _) = G.graphFromEdges [(v, v, us) | (v, us) <- orderedPairs ps]
    components = G.scc g

-- | Returns the candidate preferred by the majority of voters, if there is one.
majorityWinner :: (Eq a, Hashable a, Integral n) => Prefs a n -> Maybe a
majorityWinner (Prefs ps) = do
  let top (_, []) = Nothing
      top (n, c : _) = Just (c, n)
  let ts = HM.fromListWith (+) . catMaybes . map top  $ ps
  guard (not $ HM.null ts)
  let (winner, count) = maximumBy (comparing snd) . HM.toList $ ts
  guard (2 * count > getSum (foldMap Sum ts))
  return winner
