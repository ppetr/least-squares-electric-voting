import Control.Arrow
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import Numeric.LinearAlgebra (Matrix, (><), flatten, linearSolveSVD, toList)

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
    let c = fromIntegral count in
    [ (u, [(u, c), (v, -c)], -c)
    , (v, [(u, -c), (v, c)], c)
    ]

newtype IndexedMonoid i m = IndexedMonoid (HashMap i m)
  deriving (Eq, Show, Read)

instance (Eq i, Hashable i, Semigroup m) => Semigroup (IndexedMonoid i m) where
    IndexedMonoid m1 <> IndexedMonoid m2 =
        IndexedMonoid $ M.unionWith (<>) m1 m2

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


sumEdges' :: (Hashable a, Eq a, Num n)
         => [(a, [(a, n)], n)] -> IndexedMonoid a (IndexedMonoid a (Sum n), (Sum n))
sumEdges' = foldMap (\(c, ls, r) -> insert c (foldMap (uncurry insert . second Sum) ls, Sum r))

sumEdges :: (Hashable a, Eq a, Num n)
         => [(a, [(a, n)], n)] -> HashMap a (HashMap a n, n)
sumEdges =
    M.fromListWith (\(h1, l1) (h2, l2) -> (M.unionWith (+) h1 h2, l1 + l2))
    . map (\(v, rs, l) -> (v, (M.fromListWith (+) rs, l)))


equations' :: (Hashable a, Eq a)
           => [a] -> IndexedMonoid a (IndexedMonoid a (Sum Double), (Sum Double))
           -> (Matrix Double, Matrix Double)
equations' is m =
  ( (n >< n) [ getSum $ ilookup col (fst (ilookup row m))
             | row <- is, col <- is ]
  , (n >< 1) [ getSum $ snd (ilookup row m) | row <- is ])
  where
    n = size m

equations :: (Hashable a, Eq a)
          => [a] -> HashMap a (HashMap a Double, Double)
          -> (Matrix Double, Matrix Double)
equations indices m =
  ( (n >< n) [ findWithDefault 0 col (fst (findWithDefault (M.empty, 0) row m))
             | row <- indices, col <- indices ]
  , (n >< 1) [ snd (findWithDefault (M.empty, 0) row m) | row <- indices ])
  where
    n = M.size m
    findWithDefault v k = fromMaybe v . M.lookup k

solve' :: (Hashable a, Eq a) => IndexedMonoid a (IndexedMonoid a (Sum Double), (Sum Double)) -> [(a, Double)]
solve' m = sortBy (comparing snd) . zip is . toList . flatten . uncurry linearSolveSVD . equations' is $ m
  where
    is = indices m

solve :: (Hashable a, Eq a) => HashMap a (HashMap a Double, Double) -> [(a, Double)]
solve m = sortBy (comparing snd) . zip indices . toList . flatten . uncurry linearSolveSVD . equations indices $ m
  where
    indices = M.keys m

{-
  Example: <https://en.wikipedia.org/wiki/Condorcet_method#Example:_Voting_on_the_location_of_Tennessee's_capital>
-}
exampleTennessee :: [(Int, [Int])]
exampleTennessee =
    -- (count, choice)
    [ (42, [ memphis, nashville, chattanooga, knoxville ])
    , (26, [ nashville, chattanooga, knoxville, memphis ])
    , (15, [ chattanooga, knoxville, nashville, memphis ])
    , (17, [ knoxville, chattanooga, nashville, memphis ])
    ]
  where
    chattanooga = 0
    knoxville = 1
    memphis = 2
    nashville = 3

{-
voting :: Int -> [[Int]] -> Array (Int, Int) Int
voting dim vs = runSTArray $ pack . concatMap (\xs -> zip xs (tail xs)) $ vs
  where
    pack :: [(Int, Int)] -> ST s (STArray s (Int, Int) Int)
    pack pairs = do
        a <- newArray ((1, 1), (dim, dim)) 0
        forM_ pairs $ \(g, l) ->
            writeArray a (g, l) . (+ 1) =<< readArray a (g, l)
        return a
-}

main :: IO ()
main = do
    let system = sumEdges'
                    $ [ e | (count, order) <- exampleTennessee
                          , (u, v) <- zip order (tail order)
                          , e <- edge count u v
                      ]
    print system
    print "======"
    print $ solve' system
