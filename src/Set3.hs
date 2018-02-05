import MCPrelude

-- Ex 1

allPairs :: [a] -> [b] -> [(a,b)]
allPairs _ [] = []
allPairs [] b = []
allPairs (a:as) b = map ((,) a) b ++ allPairs as b

-- Ex 2

data Card = Card Int String

instance Show Card where
  show (Card i s) = show i ++ s

allCards :: [Int] -> [String] -> [Card]
allCards _ [] = []
allCards [] b = []
allCards (a:as) b = map (Card a) b ++ allCards as b

-- Ex 3

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ _ [] = []
allCombs _ [] _ = []
allCombs f (a:as) b = map (f a) b ++ allCombs f as b

allPairs2 :: [a] -> [b] -> [(a,b)]
allPairs2 = allCombs (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allCombs Card

 -- Ex 4

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ _ _ [] = []
allCombs3 _ _ [] _ = []
allCombs3 _ [] _ _ = []
allCombs3 f (a:as) b c = allCombs (f a) b c ++ allCombs3 f as b c

-- Ex 5

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep (f:fs) a = map f a ++ combStep fs a

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f a b = combStep (map f a) b

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f a b c = combStep (combStep (map f a) b) c
