{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- Random Number Generation

fiveRands :: [Integer]
fiveRands = map fst $ take 5 $ iterate nextRand (rand (mkSeed 1))

nextRand :: (Integer, Seed) -> (Integer, Seed)
nextRand (_, seed) = (n, newSeed)
    where
        (n, newSeed) = rand seed

-- Random Character Generation

randLetter :: Seed -> (Char, Seed)
randLetter seed = (toLetter n, newSeed)
    where (n, newSeed) = rand seed

randString3 :: String 
randString3 = map fst $ take 3 $ iterate nextRandChar (randLetter (mkSeed 1))

nextRandChar :: (Char, Seed) -> (Char, Seed)
nextRandChar (_, seed) = (n, newSeed)
    where
        (n, newSeed) = randLetter seed

