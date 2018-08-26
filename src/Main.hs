{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Control.Monad
import           Data.Coerce
import           Data.Functor.Identity

-- we start out by defining some boilerplate for
-- natural transformations and composition of functors.

type f ~> g = forall x. f x -> g x

newtype (f :. g) x = C { unC :: f (g x) }
                     deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (f :. g) where
  fmap f (C fgx) = C $ fmap f <$> fgx

-- a typeclass for adjunctions, including a proof of
-- how every pair of adjoint functors lets us derive
-- a monad instance. deriving comonad is left as an exercise ;)

class (Functor f, Functor g) => Adjunction f g | f -> g, g -> f where
  leftAdjunct :: (f x -> a) -> (x -> g a)

  rightAdjunct :: (x -> g a) -> (f x -> a)

  unit :: Identity ~> (g :. f)
  unit (Identity x) = C $ leftAdjunct id x

  counit :: (f :. g) ~> Identity
  counit (C fga) = coerce $ rightAdjunct id fga

instance (Adjunction f g) => Applicative (g :. f) where
  pure = return
  (<*>) = ap

instance (Adjunction f g) => Monad (g :. f) where
  return = unit . coerce
  m >>= f = mu . C . fmap f $ m
    -- full disclosure: this function is *not* pretty and
    -- it took some extreme hole driven development to
    -- get it to typecheck ^^
    where mu :: (g :. f) :. (g :. f) ~> (g :. f)
          mu = C . fmap (coerce . counit . C) . unC . fmap unC . unC

-- we construct the state monad as the monad induced by the
-- adjunction of the "left product" functor and the covariant
-- homfunctor. we also introduce a selection of the standard state api.

newtype LeftPair b a = LP { unLPair :: (a, b) }
                       deriving (Show, Eq, Functor)

instance Adjunction (LeftPair s) ((->) s) where
  leftAdjunct f x s = f $ LP (x, s)
  rightAdjunct g = uncurry g . unLPair

type State s a = ((->) s :. LeftPair s) a

runState :: State s a -> s -> (a, s)
runState m s = unLPair $ unC m s

get :: State s s
get = state $ \s -> (s, s)

gets :: (s -> s') -> State s s'
gets f = f <$> get

put :: s -> State s ()
put s = state $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = gets f >>= put

state :: (s -> (a, s)) -> State s a
state = C . (LP .)

-- finally we define the essential push/pop operations of stacks,
-- and take the result out for a spin!

push :: a -> State [a] ()
push = modify . (:)

pop :: State [a] (Maybe a)
pop = do
  xs <- get
  case xs of
    []      -> return Nothing
    (x:xs') -> put xs' >> return (Just x)

stackDemo :: State [Integer] (Maybe Integer)
stackDemo = do
  push 1
  push 5
  push 3
  x <- pop
  y <- pop
  return $ (+) <$> x <*> y

main :: IO ()
main = print $ runState stackDemo [] -- (8, [1])
