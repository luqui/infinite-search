----------------------------------------
-- |
-- Module    : Data.IVar
-- License   : BSD3
--
-- Maintainer  : Luke Palmer <lrpalmer@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- An implementation of nonempty searchable (compact and overt) sets, i.e. sets s 
-- which admit a total operation @search s :: (a -> Bool) -> Maybe a@.
-- 
-- Example usage:
-- 
-- > bit = pair True False
-- > cantor = sequence (repeat bit)
-- > take 5 $ search cantor (\s -> not (s !! 3) && (s !! 4))
-- >    -- gives [True, True, True, False, True]
--
-- This module is based on the paper \"Exhaustible sets in higher-type computation\"
-- by Martin Escardo, and is almost identical to the code in his his expository blog
-- post on the subject: @http:\/\/math.andrej.com\/2008\/11\/21\/a-haskell-monad-for-infinite-search-in-finite-time\/@
----------------------------------------

module Data.Searchable
    (Set, search, bigUnion, contains, member, forsome, forevery, singleton, doubleton, union)
where

import Control.Applicative
import Control.Monad (ap)

-- | @Set a@ is a nonempty searchable set of a's.
-- There is no Monoid or MonadPlus instance, since
-- we lack the ability to represent the empty set,
-- which would be the units of those structures.
newtype Set a = Set { find :: (a -> Bool) -> a }

instance Functor Set where
    fmap f s = Set (\p -> f (find s (p . f)))

instance Monad Set where
    return = Set . const
    m >>= f = bigUnion (fmap f m)

instance Applicative Set where
    pure = return
    (<*>) = ap

-- | @bigUnion ss@ is the union of all the elemens of @ss@.  In other words,
-- @x \`member\` bigUnion ss@ iff @forsome ss (\s -> x \`member\` s)@.
bigUnion :: Set (Set a) -> Set a
bigUnion ss = Set (\p -> find (find ss (\s -> forsome s p)) p)

-- | Tests whether the set contains an element.  @contains s x = forsome s (== x)@.
contains :: (Eq a) => Set a -> a -> Bool
contains s x = forsome s (== x)

-- | @member = flip contains@
member :: (Eq a) => a -> Set a -> Bool
member = flip contains
        
-- | Choose a member of the set satisfying a predicate.  
-- If @search s p = Just x@ then @p x = True@.
search :: Set a -> (a -> Bool) -> Maybe a
search s p = if p x then Just x else Nothing
    where
    x = find s p

-- | @forsome s p@ returns True iff there is some element @x@ of @s@ such that
-- @p x = True@.
forsome :: Set a -> (a -> Bool) -> Bool
forsome s p = p (find s p)

-- | @forevery s p@ returns True iff every element @x@ of @s@ satisfies @p x =
-- True@.
forevery :: Set a -> (a -> Bool) -> Bool
forevery s p = not (forsome s (not . p))

-- | @singleton x@ is the set @{x}@.
singleton :: a -> Set a
singleton = return

-- | @pair x y@ is the set @{x,y}@.
doubleton :: a -> a -> Set a
doubleton x y = Set (\p -> if p x then x else y)

-- | @x \`member\` union s t@ iff @(x \`member\` s) || (x \`member\` t)@.
union :: Set a -> Set a -> Set a
union s t = bigUnion (doubleton s t)
