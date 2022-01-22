{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- Random Number Generation

fiveRands :: [Integer]
fiveRands = nextRand 5 (mkSeed 1) 

nextRand :: Integer -> Seed -> [Integer]
nextRand cnt seed =
    if cnt == 0
        then []
        else n : nextRand (cnt-1) newSeed
    where
        (n, newSeed) = rand seed
    
-- Random Character Generation

randLetter :: Seed -> (Char, Seed)
randLetter seed = (toLetter n, newSeed)
    where (n, newSeed) = rand seed

randString3 :: String 
randString3 = nextRandChar 3 (mkSeed 1)

nextRandChar :: Integer -> Seed -> String
nextRandChar cnt seed =
    if cnt == 0
        then ""
        else n : nextRandChar (cnt-1) newSeed
    where
        (n, newSeed) = randLetter seed
