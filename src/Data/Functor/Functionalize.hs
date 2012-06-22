{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Functionalize
-- Copyright   :  (c) 2012 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains a class and instances for functionalizing functors.
--
-- Consider the following examples with their types and synonymous types:
--
-- @
--   'funfun' ('Identity' &#34;a&#34;)
--     :: 'Identity' :=> String
--      ~ String
-- @
--
-- @
--   'funfun' show
--     :: Show a => 'Arr' a :=> String
--      ~ Show a => a -> String
-- @
--
-- @
--   'funfun' ('Identity' &#34;a&#34; '<.>' show)
--     :: Show a => Compose 'Identity' ('Arr' a) :=> String
--      ~ Show a => a -> String
-- @
--
-- @
--   'funfun' ('Identity' &#34;a&#34; '<.>' 'Constant' &#39;b&#39;)
--     :: 'Compose' 'Identity' ('Constant' Char) :=> String
--      ~ Char
-- @
--
-- @
--   'funfun' ('Constant' &#39;a&#39; '<.>' 'Constant' 3)
--     :: (Num a, Monoid r) => 'Compose' ('Constant' Char) ('Constant' a) :=> r
--      ~ Char
-- @
--
-- @
--   'funfun' ('Pair' ('Identity' "a") show)
--     :: Show a => 'Product' 'Identity' ('Arr' a) :=> String
--      ~ Show a => (String, a -> String)
-- @
--------------------------------------------------------------------------------

module Data.Functor.Functionalize (
  Functionalize(..),
  (<.>),
  Arr,
  module Data.Functor.Identity,
  module Data.Functor.Constant,
  module Data.Functor.Reverse,
  module Data.Functor.Compose,
  module Data.Functor.Product,
) where

--------------------------------------------------------------------------------

import Control.Monad.Instances () -- instance Functor (Arr a)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Constant (Constant(..))
import Data.Functor.Reverse (Reverse(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Product (Product(..))
import Data.Monoid (Monoid, (<>))

--------------------------------------------------------------------------------

-- | Instances of this class can be lifted to functions.
class Functor f => Functionalize f where

  -- | (@infixr 9@) This is the function type derived from the functor @f@ with
  -- @r@ as the (possible) result type.
  type f :=> r

  -- | This function lifts the functor @f@ to a function with @r@ as the
  -- (possible) result type.
  funfun :: f r -> f :=> r

infixr 9 :=>

--------------------------------------------------------------------------------

-- | (@infixr 6@) A convenient function for composing functors
(<.>) :: (Functor f, Functor g, Monoid r) => f r -> g r -> Compose f g r
f <.> g = Compose (fmap (\s -> fmap (\t -> s <> t) g) f)
infixr 6 <.>

--------------------------------------------------------------------------------

-- | A convenient synonym for the arrow type
type Arr = (->)

--------------------------------------------------------------------------------

-- | Construct a function type from the arrow functor.
instance Functionalize (Arr a) where
  type Arr a :=> r = a -> r
  funfun f = f

-- | Leave the result type alone.
instance Functionalize Identity where
  type Identity :=> r = r
  funfun (Identity x) = x

-- | Replace the result type with the type @a@.
instance Functionalize (Constant a) where
  type Constant a :=> r = a
  funfun (Constant x) = x

-- | No effect on the functor @f@.
instance Functionalize f => Functionalize (Reverse f) where
  type Reverse f :=> r = f :=> r
  funfun (Reverse f) = funfun f

-- | Functionalize @g@ and functionalize @f@ with the functionalized @g@ as the
-- result type.
instance (Functionalize f, Functionalize g) => Functionalize (Compose f g) where
  type Compose f g :=> r = f :=> g :=> r
  funfun (Compose fg) = funfun (fmap funfun fg)

-- | Produces a pair of functionalized functors.
instance (Functionalize f, Functionalize g) => Functionalize (Product f g) where
  type Product f g :=> r = (f :=> r, g :=> r)
  funfun (Pair f g) = (funfun f, funfun g)

