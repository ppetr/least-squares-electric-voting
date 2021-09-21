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

import           Control.Monad                  ( forM_ )
import           Text.Printf                    ( printf )

import           LeastSquaresVoting
import           Types

{-
  Example: <https://en.wikipedia.org/wiki/Condorcet_method#Example:_Voting_on_the_location_of_Tennessee's_capital>
-}
exampleTennessee :: Prefs String Int
exampleTennessee = Prefs
    -- (count, ranked list)
  [ (42, [memphis, nashville, chattanooga, knoxville])
  , (26, [nashville, chattanooga, knoxville, memphis])
  , (15, [chattanooga, knoxville, nashville, memphis])
  , (17, [knoxville, chattanooga, nashville, memphis])
  ]
 where
  chattanooga = "Chattanooga"
  knoxville   = "Knoxville"
  memphis     = "Memphis"
  nashville   = "Nashville"

main :: IO ()
main = forM_ (vote exampleTennessee) $ \(candidate, value) ->
  putStrLn $ printf "%+.4f : %s" value (show candidate)
