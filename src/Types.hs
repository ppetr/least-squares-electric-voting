module Types where

-- | Preferences expressed as a elements, each being a ranked list of
-- candidates with the number of voters preferring this list.
newtype Prefs a n = Prefs { getPrefs :: [(n, [a])] }
