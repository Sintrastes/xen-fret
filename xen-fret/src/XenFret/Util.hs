module XenFret.Util where

import Diagrams.Prelude ((#))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.List

-- | Utility fnction to generate the intervals of a 
-- moment of symetry scale of a given period, generator,
-- and number of notes.
--
-- For example, mosIntervals 22 13 7 will give the
-- intervals of superpyth[7] in 22-EDO, and mosIntervals 12 7 7 
-- will give the intervals of the standard diatonic scale in 12-TET.
--
mosIntervals :: Int -> Int -> Int -> NonEmpty Int
mosIntervals period generator notes = let
      absNotes = sort $ take notes $
          (`mod` period) . (* generator) <$> [0..]

      scaleIntervals = zipWith (-) (tail absNotes) absNotes

      lastInterval = period - sum scaleIntervals
  in 
      NE.fromList $ scaleIntervals ++ [lastInterval]

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _        = False

increasing :: Ord a => [a] -> Bool
increasing []  = True
increasing [_] = True
increasing (x:y:xs) | y >= x = False
                    | otherwise = increasing (y:xs)

-- | Filter out a strictly "increasing" list.
-- (i.e. p(x[n]) implies not p(x[m]) for all m > n,
-- where m,n are indices of a list x, and p is a 
-- predicate) 
filterOutInc :: (a -> Bool) -> [a] -> [a]
filterOutInc _ [] = []
filterOutInc p (x: xs) | p x = filterOutInc p xs
                       | otherwise = x:xs

-- ** Error handling utility functions

-- | collectErrors is a utility funtion to deal with reporting multiple
-- errors from an (Either String a) context, For each of the tuples in the
-- list passed to collectErrors, if the condition on the left evaluates to
-- True, the String on the right will be included in the error message, but
-- not if the condition evaluates to false.
collectErrors :: [(Bool,String)] -> Either String a
collectErrors xs = Left ( filter fst xs #
                     \x -> "Error(s): " ++
                    foldl1 (\a b -> a++"; "++b) (map snd x)
                     ++ ".")

-- | collectErrList works similarly to collectErrors, but instead
-- of specifing an output format, it just returns a list of the
-- error messages.
collectErrList :: [(Bool,String)] -> [String]
collectErrList [] = []
collectErrList ((True ,err):xs)  = err : collectErrList xs
collectErrList ((False, _):xs) = collectErrList xs

-- | Format a list of error strings
concatErrors :: [String] -> String
concatErrors xs = "Error(s): " ++ foldl1 (\a b -> a++"; "++b) xs ++ "."
