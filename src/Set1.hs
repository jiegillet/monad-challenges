import MCPrelude
import Set4

makeRandom :: Gen Integer
makeRandom = Gen rand

-- Ex 1

fiveRands :: [Integer]
fiveRands = go 5 $ mkSeed 1
  where go 0 _ = []
        go n s = let (i, s') = rand s in i : go (n-1) s'

fiveRands2 = runGen (repRandom (replicate 5 makeRandom)) (mkSeed 1)

-- fiveRands3 = do
--   gr <- replicate 5 makeRandom
--   return $ runGen gr (mkSeed 1)

-- Ex 2

--randLetter :: Seed -> (Char, Seed)
randLetter :: Gen Char
randLetter = Gen $ \s -> let (i, s') = runGen makeRandom s in (toLetter i, s')

randLetter2 = generalA toLetter makeRandom

randString3 :: String
randString3 = go 3 $ mkSeed 1
  where go 0 _ = []
        go n s = let (c, s') = runGen randLetter s in c : go (n-1) s'

randString3' = fst $ runGen (repRandom $ replicate 3 randLetter) $ mkSeed 1

-- Ex 3

--type Gen a = Seed -> (a, Seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g = Gen $ \s -> let (a, s') = runGen g s in (f a, s')

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (*2) makeRandom

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA (+1) randEven

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (*10) makeRandom

-- Ex 4

randPair :: Gen (Char, Integer)
randPair = Gen $ \s -> let (c, s') = runGen randLetter s
                           (i, s'') = rand s'
                       in ((c, i), s'')

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair g h = Gen $ \s -> let (a, s') = runGen g s
                                  (b, s'') = runGen h s'
                              in ((a, b), s'')

randPair2 :: Gen (Char, Integer)
randPair2 = generalPair randLetter makeRandom

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f g h = Gen $ \s -> let (a, s') = runGen g s
                                 (b, s'') = runGen h s'
                             in (f a b, s'')

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 g h = generalB (,) g h

-- Ex 5

repRandom :: [Gen a] -> Gen [a]
repRandom [] = Gen $ \s -> ([], s)
repRandom (g:gs) = generalB (:) g $ repRandom gs

-- Ex 6

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f = Gen $ \s -> let (a, s') = runGen g s
                             h  = f a
                             (b, s'') = runGen h s'
                         in (b, s'')

mkGen :: a -> Gen a
mkGen a = Gen $ \s -> (a, s)

-- Set4 - Ex 2

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f a b = genTwo a (\q -> genTwo b (mkGen . f q))

-- generalA :: (a -> b) -> Gen a -> Gen b
-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- mkGen :: a -> Gen a
--
-- repRandom2 :: [Gen a] -> Gen [a]
-- repRandom2 []     = mkGen []
-- repRandom2 (g:gs) = genTwo g (\q -> genTwo (repRandom2 gs) (mkGen . (q:)))
