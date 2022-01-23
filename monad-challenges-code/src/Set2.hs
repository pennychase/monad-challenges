{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

-- Maybe data type and instances

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

instance Eq a => Eq (Maybe a) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    Nothing == Just _ = False
    Just _ == Nothing = False

-- Safe functions

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x (y:ys) = 
    if x == fst y 
        then Just (snd y)
        else lookupMay x ys

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x y =
    if y == 0
        then Nothing
        else Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = go xs x
    where
        go [] max = Just max
        go (y:ys) max =
            if y > max
                then go ys y
                else go ys max

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = go xs x
    where
        go [] min = Just min
        go (y:ys) min =
            if y < min
                then go ys y
                else go ys min

-- Chains of failing computations

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greek str =
    case lookupMay str greek of
        Nothing -> Nothing
        Just xs ->
            case tailMay xs of
                Nothing -> Nothing
                Just xss -> 
                    case maximumMay xss of
                        Nothing -> Nothing
                        Just m -> 
                            case headMay xs of
                                Nothing -> Nothing
                                Just n ->
                                    case divMay (fromIntegral m) (fromIntegral n) of
                                        Nothing -> Nothing
                                        Just r -> Just r

