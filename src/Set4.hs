module Set4 where

import MCPrelude
import Set2

-- Ex 1

--genTwo, link ::  m a -> (a -> m b) -> m b
--generalB, yLink :: (a -> b -> c) -> m a -> m b -> m c

-- Ex 2: See Set1

-- Ex 3

class Monad' m where
  bind' :: m a -> (a -> m b) -> m b
  return' :: a -> m a

yLink :: (Monad' m) => (a -> b -> c) -> m a -> m b -> m c
yLink f a b = bind' a (\q -> bind' b (return' . f q))

-- Ex 4

instance Monad' Maybe' where
  bind' = link
  return' = mkMaybe

instance Monad' [] where
  bind' = flip concatMap
  return' = (:[])

newtype Gen a = Gen {runGen:: Seed -> (a, Seed)}

instance Monad' Gen where
  bind' g f = Gen $ \s -> let (a, s') = runGen g s
                              h  = f a
                              (b, s'') = runGen h s'
                          in (b, s'')
  return' a = Gen $ \s -> (a, s)

-- Ex 5

sequence' :: (Monad' m) => [m a] -> m [a]
sequence' = undefined

liftM2 :: (Monad' m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f a b = bind' a (\q -> bind' b (return' . f q))

(=<<<) :: (Monad' m) => (a -> m b) -> m a -> m b
(=<<<) = flip bind'

join :: (Monad' m) => m (m a) -> m a
join = undefined

ap :: (Monad' m) => m (a -> b) -> m a -> m b
ap f a = undefined

liftM3 :: (Monad' m) =>  (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f a b c = (return' . f) =<<< a `ap` b `ap` c
