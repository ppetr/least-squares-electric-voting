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

{-# OPTIONS_GHC -Wall #-}
module LeastSquaresVoting
  ( vote
  ) where

import           Control.Arrow
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as M
import           Data.Hashable                  ( Hashable(..) )
import           Data.List                      ( sortBy )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Sum(..) )
import           Data.Ord                       ( comparing )
import           Numeric.LinearAlgebra          ( (><)
                                                , Matrix
                                                , flatten
                                                , linearSolveSVD
                                                , toList
                                                )

import           Types

-- | Let _(u, v)_ be a pair of candidates in a voter's ordered list. This means
-- that the voter's intent is that _u_ is ideally ranked a unit _D_ (1V) higher
-- than _v_. When their potentials are _U_ and _V_, the difference from the intent
-- will be _(U-V)-1_.
--
-- The **heat** (corresponding to the heat dissipated by an electrical
-- resistor) is the square of the difference multiplied by a unit conductivity
-- _G_ (which represents the voter's ability to influence the outcome).
--
-- ((U-V)-D)^2 G.
--
-- Partial derivatives
--   d/dU ... =   2 G (U-V-D)
--   d/dV ... = - 2 G (U-V-D)
edge :: (Integral c, Num n) => c -> a -> a -> [(a, [(a, n)], n)]
edge count u v =
  let c = fromIntegral count
  in  [(u, [(u, c), (v, -c)], -c), (v, [(u, -c), (v, c)], c)]

newtype IndexedMonoid i m = IndexedMonoid (HashMap i m)
  deriving (Eq, Show, Read)

instance (Eq i, Hashable i, Semigroup m) => Semigroup (IndexedMonoid i m) where
  IndexedMonoid m1 <> IndexedMonoid m2 = IndexedMonoid $ M.unionWith (<>) m1 m2

instance (Eq i, Hashable i, Monoid m) => Monoid (IndexedMonoid i m) where
  mempty = IndexedMonoid M.empty

insert :: (Eq i, Hashable i, Semigroup m) => i -> m -> IndexedMonoid i m
insert i m = IndexedMonoid $ M.singleton i m

ilookup :: (Eq i, Hashable i, Monoid m) => i -> IndexedMonoid i m -> m
ilookup i (IndexedMonoid m) = fromMaybe mempty $ M.lookup i m

size :: (Hashable i) => IndexedMonoid i m -> Int
size (IndexedMonoid m) = M.size m

indices :: (Hashable i) => IndexedMonoid i m -> [i]
indices (IndexedMonoid m) = M.keys m

type LinearSystem a n = IndexedMonoid a (IndexedMonoid a (Sum n), (Sum n))

sumEdges :: (Hashable a, Eq a, Num n) => [(a, [(a, n)], n)] -> LinearSystem a n
sumEdges = foldMap
  (\(c, ls, r) -> insert c (foldMap (uncurry insert . second Sum) ls, Sum r))


equations
  :: (Hashable a, Eq a)
  => [a]
  -> LinearSystem a Double
  -> (Matrix Double, Matrix Double)
equations is m =
  ( (n >< n)
    [ getSum $ ilookup col (fst (ilookup row m)) | row <- is, col <- is ]
  , (n >< 1) [ getSum $ snd (ilookup row m) | row <- is ]
  )
  where n = size m

solve :: (Hashable a, Eq a) => LinearSystem a Double -> [(a, Double)]
solve m =
  sortBy (comparing snd)
    . zip is
    . toList
    . flatten
    . uncurry linearSolveSVD
    . equations is
    $ m
  where is = indices m

vote :: (Eq a, Hashable a, Integral n) => Prefs a n -> [(a, Double)]
vote (Prefs []) = []
vote (Prefs prefs@((_, cs) : _)) | null edges = zip cs (repeat 0)
                                 | otherwise  = solve . sumEdges $ edges
 where
  edges =
    [ e
    | (count, order) <- prefs
    , (u    , v    ) <- zip order (tail order)
    , e              <- edge count u v
    ]
