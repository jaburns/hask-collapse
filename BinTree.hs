
{-# LANGUAGE NoMonomorphismRestriction #-}

module BinTree (
    BinTree(..)    
    ) where

import qualified Data.Foldable as F 
import Data.Monoid

data BinTree x = Node (BinTree x) (BinTree x)
               | Leaf x
               deriving (Eq, Ord, Read)

instance (Show x) => Show (BinTree x) where
    show (Node a b) = "{" ++ show a ++ "," ++ show b ++ "}"
    show (Leaf x)   = show x

instance Functor BinTree where
    fmap f (Node a b) = Node (fmap f a) (fmap f b)
    fmap f (Leaf x)   = Leaf (f x)

instance F.Foldable BinTree where
    foldr f z (Node a b) = F.foldr f (F.foldr f z b) a
    foldr f z (Leaf x)   = f x z

