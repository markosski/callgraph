module Stack where

import Data.Maybe

newtype Stack a = Stack [a] deriving Show

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs)= Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

peek :: Stack a -> (Maybe a)
peek (Stack []) = Nothing
peek (Stack (x:xs)) = Just x