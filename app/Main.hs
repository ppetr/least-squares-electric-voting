import Control.Monad (forM_)
import Text.Printf (printf)

import LeastSquaresVoting

{-
  Example: <https://en.wikipedia.org/wiki/Condorcet_method#Example:_Voting_on_the_location_of_Tennessee's_capital>
-}
exampleTennessee :: [(Int, [String])]
exampleTennessee =
    -- (count, choice)
    [ (42, [ memphis, nashville, chattanooga, knoxville ])
    , (26, [ nashville, chattanooga, knoxville, memphis ])
    , (15, [ chattanooga, knoxville, nashville, memphis ])
    , (17, [ knoxville, chattanooga, nashville, memphis ])
    ]
  where
    chattanooga = "Chattanooga"
    knoxville = "Knoxville"
    memphis = "Memphis"
    nashville = "Nashville"

main :: IO ()
main = forM_ (vote exampleTennessee) $ \(candidate, value) ->
        putStrLn $ printf "%+.4f : %s" value (show candidate)
