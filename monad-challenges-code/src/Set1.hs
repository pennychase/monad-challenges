{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- Define generalA to capture pattern
-- Rewrite exercises using Gen A

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
fiveRands = genNRand 5 rand
    
-- Random Character Generation

randLetter :: Gen Char 
randLetter = generalA toLetter rand

randString3 :: String 
randString3 = genNRand 3 randLetter

-- More Generators

randEven :: Gen Integer
randEven = generalA (2 *) rand 

randOdd :: Gen Integer
randOdd = generalA (1 +) randEven

randTen :: Gen Integer
randTen = generalA (10 *) rand

testResult = product $ map (fst . \f -> f (mkSeed 1)) [randEven, randOdd, randTen]

-- Generalizing random pairs

randPair :: Gen (Char, Integer)
randPair s =
    let
        (c, s') = randLetter s
        (n, s'') = rand s'
    in ((c, n), s'')

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair f g s =
    let
        (v1, s') = f s
        (v2, s'') = g s'
    in ((v1, v2), s'')

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB comp f g s =
    let
        (v1, s') = f s
        (v2, s'') = g s'
    in (comp v1 v2, s'')

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 f g s = generalB (,) f g s

randPair2 :: Gen (Char, Integer)
randPair2 = generalPair2 randLetter rand
