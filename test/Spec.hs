{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio (approxRational)
import qualified Data.Set as S
import Data.Tuple (swap)
import Test.QuickCheck
import System.Exit

import LeastSquaresVoting
import Properties

-- | Helper object to generate preferences for testing.
newtype TestPrefs = TestPrefs {
    getTestPrefs :: M.Map [Char] Int
    }
  deriving (Show)

toPrefs :: TestPrefs -> Prefs Char Int
toPrefs = Prefs . map swap . M.toList . getTestPrefs

-- | Runs the last squares vote on the test preferences, rounds the results to
-- avoid double-precision artifacts and sorts the candidates by rank and
-- (secondarily) lexicographically.
normVote :: TestPrefs -> [Char]
normVote =
    map snd . L.sort . map (\(c, r) -> (approxRational r 1E-10, c))
    . vote . toPrefs

candidates :: TestPrefs -> [Char]
candidates = maybe [] fst . M.lookupMin . getTestPrefs

removeCandidate :: Char -> TestPrefs -> TestPrefs
removeCandidate c =
    TestPrefs . M.mapKeysWith (+) (L.delete c) . getTestPrefs

instance Arbitrary TestPrefs where
    arbitrary = TestPrefs . M.fromListWith (+) <$> do
        size <- getSize
        let candidates = ceiling (sqrt $ fromIntegral size)
        let list = take candidates ['a'..]
        len <- choose (1, size)
        replicateM len ((,) <$> shuffle list <*> choose (1, 100))
    shrink (TestPrefs ps) | M.null ps = []
    shrink (TestPrefs ps) = map TestPrefs $
        -- Drop one of the preferences.
        [ M.deleteAt i ps | i <- [0..(M.size ps - 1)] ]
        -- Remove one of the candidates.
        ++ [ M.mapKeysWith (+) (L.delete c) ps | c <- fst (M.findMin ps) ]
        -- Reduce one candidate's votes.
        ++ [ M.insert c v' ps | (c, v) <- M.toList ps, v' <- shrink v ]

-- | Example: <https://en.wikipedia.org/wiki/Condorcet_method#Example:_Voting_on_the_location_of_Tennessee's_capital>
prop_Tennessee :: Property
prop_Tennessee =
    map fst result === [nashville, chattanooga, memphis, knoxville]
  where
    result = vote (Prefs
        [ (42, [ memphis, nashville, chattanooga, knoxville ])
        , (26, [ nashville, chattanooga, knoxville, memphis ])
        , (15, [ chattanooga, knoxville, nashville, memphis ])
        , (17, [ knoxville, chattanooga, nashville, memphis ])
        ] :: Prefs String Int)
    chattanooga = "Chattanooga"
    knoxville = "Knoxville"
    memphis = "Memphis"
    nashville = "Nashville"

-- | A counter-example for the majority criterion.
prop_not_majority :: Property
prop_not_majority = mwinner'm =/= Just winner
  where
    winner = fst . head $ vote example
    mwinner'm = majorityWinner example
    example = Prefs [(6, "abc"), (5, "bca")] :: Prefs Char Int

-- | A counter-example for local idependence of irrelevant alternatives.
prop_not_liia :: Property
prop_not_liia = head result =/= head (normVote withoutLeast)
  where
    example = TestPrefs . M.fromList $ [("abc", 6), ("bca", 5)]
    result = normVote example
    withoutLeast = removeCandidate (last result) example

return []
main :: IO ()
main = do
    success <- $quickCheckAll
    unless success $ exitWith (ExitFailure 1)
