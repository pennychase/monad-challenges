{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- Define generalA to capture pattern
-- Rewrite exercises

type Gen a = Seed -> (a, Seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g s = 
    let
        (x, s') = g s
    in (f x, s')

genNRand :: Int -> Gen a -> [a]
genNRand n f = take n $ map fst rands
    where 
        rands = f (mkSeed 1) : map (f . snd) rands

-- Random Number Generation

fiveRands :: [Integer]
fiveRands = take 5 $ map fst rands
    where
        rands :: [(Integer, Seed)]
        rands = (0, mkSeed 1) : map (rand . snd) rands
    
-- Random Character Generation

randLetter :: Gen Char 
randLetter = generalA toLetter rand

randString3 :: String 
randString3 = nextRandChar 3 (mkSeed 1)

nextRandChar :: Integer -> Seed -> String
nextRandChar cnt seed =
    if cnt == 0
        then ""
        else n : nextRandChar (cnt-1) newSeed
    where
        (n, newSeed) = randLetter seed

-- More Generators

randEven :: Gen Integer 
randEven seed = (2 * n, newSeed)
    where
        (n, newSeed) = rand seed

randOdd :: Gen Integer
randOdd seed = (1 + n, newSeed)
    where
        (n, newSeed) = randEven seed

randTen :: Gen Integer
randTen seed = (10 * n, newSeed)
    where
        (n, newSeed) = rand seed

testResult = product $ map (fst . \f -> f (mkSeed 1)) [randEven, randOdd, randTen]



randEven' :: Gen Integer
randEven' = generalA (2 *) rand 

randOdd' :: Gen Integer
randOdd' = generalA (1 +) randEven'

randTen' :: Gen Integer
randTen' = generalA (10 *) rand


