module XenFret.Util where

import Diagrams.Prelude ((#))

xor True False = True
xor False True = True
xor _ _        = False

increasing []  = True
increasing [_] = True
increasing (x:y:xs) | y >= x = False
                    | otherwise = increasing (y:xs)

-- | Filter out a strictly "increasing" list.
-- (i.e. p(x[n]) implies not p(x[m]) for all m > n,
-- where m,n are indices of a list x, and p is a 
-- predicate) 
filterOutInc :: (a -> Bool) -> [a] -> [a]
filterOutInc p [] = []
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
collectErrList ((True,err):xs)  = err : collectErrList xs
collectErrList ((False,err):xs) = collectErrList xs

-- | Format a list of error strings
concatErrors :: [String] -> String
concatErrors xs = "Error(s): " ++ foldl1 (\a b -> a++"; "++b) xs ++ "."
