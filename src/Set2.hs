module Set2 where

import MCPrelude

-- Ex 1

data Maybe' a = Nothing' | Just' a

instance Show a => Show (Maybe' a) where
  show Nothing' = "Nothing"
  show (Just' a) = "Just " ++ show a

-- Ex 2

headMay :: [a] -> Maybe' a
headMay [] = Nothing'
headMay x = Just' $ head x

tailMay :: [a] -> Maybe' [a]
tailMay [] = Nothing'
tailMay x = Just' $ tail x

lookupMay :: Eq a => a -> [(a, b)] -> Maybe' b
lookupMay x lst
  | null z = Nothing'
  | otherwise = Just' $ snd $ head z
  where z = filter (\(a,_)->a==x) lst

maximumMay :: Ord a => [a] -> Maybe' a
maximumMay [] = Nothing'
maximumMay x = Just' $ maximum x

minimumMay :: Ord a => [a] -> Maybe' a
minimumMay [] = Nothing'
minimumMay x = Just' $ minimum x

divMay :: (Eq a, Fractional a) => a -> a -> Maybe' a
divMay _ 0 = Nothing'
divMay q d = Just' $ q/d

-- Ex 3

queryGreek :: GreekData -> String -> Maybe' Double
queryGreek d s = case lookupMay s d of
  Nothing' -> Nothing'
  Just' xs  -> case tailMay xs of
    Nothing' -> Nothing'
    Just' t   -> case maximumMay t of
      Nothing' -> Nothing'
      Just' m   -> let Just' h = headMay xs
                  in divMay (fromIntegral m) (fromIntegral h)

-- Ex 4

chain :: (a -> Maybe' b) -> Maybe' a -> Maybe' b
chain f m = case m of
  Nothing' -> Nothing'
  Just' a  -> f a

link :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe' Double
queryGreek2 d s = let xs = lookupMay s d
                      m = chain maximumMay $ chain tailMay xs
                      h = chain headMay xs
                      divMay2 q d = divMay (fromIntegral q) (fromIntegral d)
                  in  chain (\q -> chain (divMay2 q) h) m

queryGreek3 :: GreekData -> String -> Maybe' Double
queryGreek3 d s = lookupMay s d     `link`
                  \xs -> tailMay xs `link`
                  maximumMay        `link`
                  \m  -> headMay xs `link`
                  \h -> divMay (fromIntegral m) (fromIntegral h)

-- Ex 5

mkMaybe :: a -> Maybe' a
mkMaybe = Just'

yLink :: (a -> b -> c) -> Maybe' a -> Maybe' b -> Maybe' c
yLink f a b = link a (\m -> link b (mkMaybe . f m))

addSalaries :: [(String, Integer)] -> String -> String -> Maybe' Integer
addSalaries s a b = yLink (+) (lookupMay a s) (lookupMay b s)

-- Ex 6

tailProd :: Num a => [a] -> Maybe' a
tailProd = chain (mkMaybe . product) . tailMay

tailSum :: Num a => [a] -> Maybe' a
tailSum = chain (mkMaybe . sum) . tailMay

transMaybe :: (a -> b) -> Maybe' a -> Maybe' b
transMaybe f = chain (mkMaybe . f)

tailProd2 :: Num a => [a] -> Maybe' a
tailProd2 = transMaybe product . tailMay

tailSum2 :: Num a => [a] -> Maybe' a
tailSum2 = transMaybe sum . tailMay

tailMax :: Ord a => [a] -> Maybe' (Maybe' a)
tailMax = transMaybe maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe' (Maybe' a)
tailMin = transMaybe minimumMay . tailMay

combine :: Maybe' (Maybe' a) -> Maybe' a
combine (Just' (Just' x)) = Just' x
combine _ = Nothing'

tailMax2 :: Ord a => [a] -> Maybe' a
tailMax2 = combine . transMaybe maximumMay . tailMay

tailMin2 :: Ord a => [a] -> Maybe' a
tailMin2 = combine . transMaybe minimumMay . tailMay
