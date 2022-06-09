
module XenFret.TemperamentTheory where

import Data.Proxy
import GHC.TypeLits
import Data.Ratio
import Data.Semigroup
import Data.List
import Data.Group

-- | A Just intonation interval is just a rational
-- number.
type JI = Rational

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

newtype Prime = Prime Int

instance Show Prime where
  show (Prime n) = show $ primes !! n

-- Q: With regard to the rank question below, I'm not sure
-- if a monzo has to be the same rank exactly as a val
-- in order to calculate the dot product. Might
-- be an inequality.
data Monzo = Monzo [Int]

-- Would it be useful to have n here for the rank?
data Val = Val [Int]

inVal :: Monzo -> Val -> Int
inVal (Monzo xs) (Val ys) = sum $ zipWith (*) xs ys 

instance Show Val where
  show (Val xs) = "⟨" ++ intercalate " " (fmap show xs) ++ "]"

data SVal = SVal [(Int, Int)]

instance Show SVal where
  show (SVal xs) = intercalate "." (fmap (show . fst) xs) ++ " " ++
      "⟨" ++ intercalate " " (fmap (show . snd) xs) ++ "]"

-- | Calculate the patent val for the given EDO
patentVal :: Int -> Val
patentVal edo = undefined

-- | Calculate the parent val for a given subgroup and 
patentSVal :: [Int] -> Int -> SVal
patentSVal subgroup edo = undefined

newtype Frequency = Frequency Double

newtype Cents = Cents Double

type AbstractTemperament note =
     note -> Frequency

data Rank2Temperament (n :: Nat) where
  Rank2Note :: KnownNat n => Proxy n -> Int -> Int -> Rank2Temperament n

instance Eq (Rank2Temperament n) where
  (Rank2Note nP x y) == (Rank2Note _ x' y') =
     let (d , r)  = x  `divMod` period
         (d', r') = x' `divMod` period
     in d == d' && r + y == r' + y'
        where period :: Int = fromInteger $ natVal nP

instance Show (Rank2Temperament n) where
  show (Rank2Note _ x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Semigroup (Rank2Temperament n) where
  (Rank2Note nP x y) <> (Rank2Note _ x' y') = 
      let (d, r) = (x + x') `divMod` period
       in Rank2Note nP r (d + y + y')
    where period :: Int = fromInteger $ natVal nP

instance KnownNat n => Monoid (Rank2Temperament n) where
  mempty = Rank2Note (Proxy @n) 0 0

instance KnownNat n => Group (Rank2Temperament n) where
  invert (Rank2Note nP x y) = Rank2Note nP (-x) (-y)