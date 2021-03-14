{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
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

instance Arbitrary TestPrefs where
    arbitrary = TestPrefs . M.fromListWith (+) <$> do
        size <- getSize
        let list = take size ['a'..]
        len <- choose (1, size)
        replicateM len ((,) <$> shuffle list <*> choose (1, 1000))
    shrink (TestPrefs ps) | M.null ps = []
    shrink (TestPrefs ps) = map TestPrefs $
        -- Drop one of the preferences.
        [ M.deleteAt i ps | i <- [0..(M.size ps - 1)] ]
        -- Remove one of the candidates.
        ++ [ M.mapKeysWith (+) (L.delete c) ps | c <- fst (M.findMin ps) ]
        -- Reduce one candidate's votes.
        ++ [ M.insert c v' ps | (c, v) <- M.toList ps, v' <- shrink v ]

prop_not_majority :: Property
prop_not_majority = mwinner'm =/= Just winner
  where
    winner = fst . head $ vote example
    mwinner'm = majorityWinner example
    example = Prefs [(6, "abc"), (5, "bca")] :: Prefs Char Int

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

return []
main :: IO ()
main = do
    success <- $quickCheckAll
    unless success $ exitWith (ExitFailure 1)
