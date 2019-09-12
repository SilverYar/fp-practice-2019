module Task3_2 where

import Todo(todo)


data ReverseList a = RNil | RCons (ReverseList a) a

toList RNil = []
toList (RCons xs x) = (toList xs) ++ [x]

toRList [] = RNil
toRList x = RCons (toRList $ init x) (last x)


instance (Show a) => Show (ReverseList a) where
    show l = show (toList l)

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) _ RNil = False
    (==) RNil _ = False
    (==) (RCons a1 b1) (RCons a2 b2) = a1 == a2 && b1 == b2

instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil _ = True
    (<=) _ RNil = False
    (<=) (RCons a1 b1) (RCons a2 b2) = b1 <= b2 || a1 <= a2

instance Monoid (ReverseList a) where
  mempty = RNil
  mappend RNil y = y
  mappend x RNil = x
  mappend x (RCons y z) = RCons (mappend x y) z

instance Semigroup (ReverseList a) where
  (<>) = mappend
 
instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap f (RCons xs x) = RCons ( fmap f xs ) (f x)