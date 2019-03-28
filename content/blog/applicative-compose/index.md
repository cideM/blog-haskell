---
title: Compose for Applicative
date: "2019-03-27"
---

## The Problem

```haskell
instance (Applicative fa, Applicative fb) => Applicative (Compose fa fb) where
    (<*>) :: Compose fa fb (a -> b) -> Compose fa fb a -> Compose fa fb b
    Compose x <*> Compose y = Compose ((<*>) <$> x <*> y)
```
_Functor types are named fa, fb, and so on. If you see an f, it's a function, both on the type and the data level. I refer to fa and fb as functors since the applicative type class requires those things to be functors._

When I went through _Haskell From First Principle_ the first time, I couldn't make sense of the above code. I think I more or less understand it now.

Creating instances for functors without additional nesting is arguably a lot easier since in `fmap f fa` the `f` is a plain function. It's not hidden inside a data structure. That lets you focus entirely on the functor `fa`. Ultimately you just apply `lift` often enough so that `f` is applied to the value inside the functor `fa`.

With the applicative instance it's fundamentally the same problem, just that the function is also wrapped inside something else.

What helped me understand how the instance works is working through it layer by layer.

## A Concrete Approach

#### One layer

The code below shows a function `(+) 2` wrapped in a `Maybe`. We apply it to a value (wrapped in a `Maybe`) by using `<*>`. Simple enough.

`> Just ((+) 2) <*> Just 5`

The type signature for `<*>` here is `fa (a -> b) -> fa a -> fa b` where `fa` is `Maybe`.

#### Two layers

Let's up the ante a bit and wrap both the function and the value in another `Maybe`.

`> Just (Just ((+) 2)) <*> Just (Just 5)`

This does not work and even without understanding category theory it seems plausible that we can't just add another layer and expect the original to work. After all the types here would be `fa (fb (a -> b)) -> fa (fb a) -> fa (fb b)`, with `fa` and `fb` both `Maybe` -- and that's just not something plain `<*>` can handle.

So what's the #1 solution for manipulating nested stuff in Haskell? Use `lift` or one of its cousins.

`> liftA2 (<*>) (Just (Just ((+) 2))) (Just (Just 5))`

We lift `<*>` over both values. I like to think of it like doing exactly the same thing as before, just one level deeper.

## Back to the Abstract Approach

How does `liftA2` help us make sense of the instance code though? 

```haskell
instance (Applicative fa, Applicative fb) => Applicative (Compose fa fb) where
    (<*>) :: Compose fa fb (a -> b) -> Compose fa fb a -> Compose fa fb b
    Compose x <*> Compose y = Compose ((<*>) <$> x <*> y)
```

The first part `(<*>) <$> x` written without infix notation and `fmap` instead of its symbol synonym is `fmap (<*>) x`. We map the `<*>` over the `x`. If you look at the type signature above, we just apply `<*>` to the `g (a -> b)` part. 

```haskell
instance (Applicative fa, Applicative fb) => Applicative (Compose fa fb) where
    (<*>) :: Compose fa fb (a -> b) -> Compose fa fb a -> Compose fa fb b
                    --  ^^^^^^^^^^^ This is the first argument to <*>
    Compose x <*> Compose y = 
        -- fa' :: fa (fb a -> fb b)
        --            ^^^^ The 2nd, missing, argument to <*>
        let fa' = fmap (<*>) x
        in ???
```

The `<*>` only needs its 2nd argument now, which is a functor with a value inside it. And we have something like that **inside** our `y` (`y` is `fa fb b` and therefore the missing argument to `<*>` is the `fb b` part inside the `fa`). How can we apply a function inside a functor to a value inside a functor? `<*>`! And that's how we arrive at the 2nd part.

```haskell
instance (Applicative fa, Applicative fb) => Applicative (Compose fa fb) where
    (<*>) :: Compose fa fb (a -> b) -> Compose fa fb a -> Compose fa fb b
    Compose x <*> Compose y = 
        let fa' = fmap (<*>) x
        in fa' <*> y
```

## TL;DR (Don't Skip This!)

The implementation of `<*>` with a mix of infix notation and symbols is visually hard to parse. The implementation on [hackage](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Functor.Compose.html#line-112), with `liftA2`, makes the concept a lot more obvious. We're lifting `<*>` through the additional level of nesting.

```haskell
::    fb (a -> b) ->    fb a ->    fb b
:: fa fb (a -> b) -> fa fb a -> fa fb b
```

If you align the type signatures of `<*>` and `Compose x <*> Compose y` you can see the similarities. It's the exact same operation one level deeper for both arguments, hence the use of `liftA2`.
