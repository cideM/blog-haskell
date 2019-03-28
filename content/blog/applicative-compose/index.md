---
title: Compose for Applicative
date: "2019-03-27"
---

## The Problem

```haskell
instance (Applicative f, Applicative g) => Applicative (Compose f g) where

    pure x = Compose (pure (pure x))

    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
```

When I went through _Haskell From First Principle_ the first time, I couldn't make sense of the above code. I think I more or less understand it now.

Creating instances for functors is arguably a lot easier since in `fmap f fa` the `f` is a plain function. It's not hidden inside a data structure. That lets you focus entirely on the functor `fa`. Ultimately you just apply `lift` often enough so that `f` is applied to the value inside the functor `fa`.

With the applicative instance it's fundamentally the same problem, just that the function is also wrapped inside something else.

What helped me understand how the instance works is working through it layer by layer.

## A Concrete Approach

#### One layer

The code below shows a function `(+) 2` wrapped in a `Maybe`. We apply it to a value (wrapped in a `Maybe`) by using `<*>`. Simple enough.
`> Just ((+) 2) <*> Just 5`

#### Two layers

Let's up the ante a bit and wrap both the function and the value in another `Maybe`.

`> Just (Just ((+) 2)) <*> Just (Just 5)`

This does not work and even without understand category theory it seems plausible that we can't just add another layer and except the original to work.

What's the #1 solution for manipulating nested stuff in Haskell? Somehow use `lift`.

`> liftA2 (<*>) (Just (Just ((+) 2))) (Just (Just 5))`

We lift `<*>` over both values. I like to think of it like doing exactly the same thing as before (`<*>`) just one level deeper.

## Back to the Abstract Approach

How does `liftA2` help us make sense of the instance code? 

```haskell
instance (Applicative f, Applicative g) => Applicative (Compose f g) where

    pure x = Compose (pure (pure x))

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
```

The first part `(<*>) <$> f` written without infix notation and `fmap` instead of its symbol synonym is `fmap (<*>) f`. We map the `<*>` over the `f`. If you look at the type signature above, we just applied `<*>` to the `g (a -> b)` part. That gives us `f (fa -> fb)` instead of `f g (a -> b)` with `func` being `<*>` with the first argument.

```haskell
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    Compose f <*> Compose x = let functionInFWithFirstArgument :: f (fa -> fb) = fmap (<*>) f
    		              in ???		  
```

The `<*>` only needs its 2nd argument now, which is a functor with a value inside it. And we have something like that **inside** our `x` (`x` is `f g b` and therefore the missing argument to `<*>` is the `g b` part inside the `f`). How can we apply a function to a value inside something else? `<*>`! And that's how we arrive at the 2nd part.

```haskell
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    Compose f <*> Compose x = let functionInFWithFirstArgument :: f (fa -> fb) = fmap (<*>) f
    		              in functionInFWithFirstArgument <*> x
```

This brings me back to `liftA2`.

`> liftA2 (<*>) (Just (Just ((+) 2))) (Just (Just 5))`

The implementation of `<*>` with a mix of infix notation and symbols is visually hard to parse. The implementation on [hackage](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Functor.Compose.html#line-112), with `liftA2` makes the concept a lot more obvious.  

## TL;DR (Don't Skip This!)

`f (a -> b) -> f a -> f b` is our normal `<*>`. We're faced with exactly that, but both arguments nested inside another structure `f g (a -> b) -> f g a -> f g b`. So we simply lift `<*>` over those structures. We take the same old `<*>` and apply it to the arguments it expects just we do the whole thing inside those structures.
