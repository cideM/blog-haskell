---
title: Learning Haskell Lenses
date: "2019-01-12"
---

I often hate Haskell but at the same time I can't stop learning it because it would feel like giving up. Learning new things in Haskell is often one of the most frustrating experiences I can think of. I stare at code that seems completely impenetrable and the category theory jargon, together with super descriptive single letter variables, feels like a concerted effort at obfuscation. Use it in papers, when discussing among equals, but not in tutorials. You've invested effort into creating a tutorial, why not go the extra mile and use descriptive names for us sub-9000 IQ folks. /rant

I'll use this post as a dumping ground for random thoughts while trying to learn lenses. I apologize for the language.

# Naming

I'm really grateful that stuff like [the "lens over tea" writeup](https://artyom.me/lens-over-tea-1) exists. Really, I mean it. Yet at the same time...

```haskell
data Storey x f a = Storey x (f a) -- ^ f a is functor a
  deriving Show


fmap f        (x, a) =     (x,) $      f a
fmap f (Storey x  a) = Storey x $ fmap f a
-- ^ f a is a function applied to a. a is (f a) from above.
```

The two code snippets are about 2 viewport heights apart (I added the comments). Stuff like this puts me in instant rage mode. It's like someone is stabbing me with a tiny knife - it won't kill me but it still inflicts a lot of pain on a constant basis. Everytime I see an `f` I need to check if it's a function or a functor. It's especially confusing if, as here, what was called `f a` (functor) is now `a`, and `f` is just a function.

Occasionally I see people refer to `f a` from a type signature as `fa` in data land. That's nice. It makes the connection between both very clear, while still being general and abstract. Let's try again:

```haskell
data Storey x f a = Storey x (f a)
  deriving Show


fmap fn        (x, a ) =     (x,) $      fn  a
fmap fn (Storey x  fa) = Storey x $ fmap fn fa
```

This makes the connection between `f a` from the data declaration and `fa` from the function clear. It also makes it obvious that `fn` is something new, something that has nothing to do with the data declaration.

I don't care if every PhD-holding Haskell-writing rocket scientist immediately knows and understands that the convention is for `f` to be functor in types and function everywhere else. And I realize that the linked article isn't even supposed to be a tutorial and that the author said that

> this post is an exploration more than an explanation, so don't expect the text to be structured the way normal blog posts are. If I knew how to structure it all in such a way that it wouldn't be a tangled braindump, I would've done that.

I'm using it as an example for something that is pervasive throughout Haskell.
In tutorials, in SO posts, on reddit. I know everyone means well and I respect
that and am grateful. But this post is what learning Haskell _often feels like
**to me**_.

## Success Is Commemorated, Failure Merely Remembered

Hey, at least I'll be remembered :|! The post eventually includes this section, where you're supposed to test your understanding. Which, by the way, is amazing! Tutorials (sorry I'll just call it tutorial from now on) without exercises are useless.

```haskell
{-# LANGUAGE RankNTypes, TupleSections #-}

import Control.Applicative

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 = _
```

So let's see. I could get the `a` out of the tuple and apply it to the modifier function. That gives me `f b`, or a functor with a `b`. Then I could map a function over that result (functor), and turn the inner `b` into `(b, x)`. It would work, and it's 100% not what I'm supposed to do (BIG PLOT TWIST AHEAD!).

```haskell
_1 modifierFn (first, second) =
  let result = modifierFn first
   in fmap (, second) result
```

or

```haskell
_1 modifierFn (first, second) = (, second) <$> modifierFn first
```

The type checker is happy, at least that I got right. The tuple section came from hlint by the way. I do know that it's a thing and it's not hard to use but I put that in the "stabbing with a tiny knife" category. Layers of syntactic sugar eventually add up to obscure code (insert quote about Golang here).

The above function doesn't feel right, because I'm sure there's a clever way to make it ~~less explicit and harder to understand~~ more idiomatic.

After staring at it for like 2 hours or so and eventually trying random code with `Compose` I looked at Github repos. It seems no one is using `Compose` for `_1` and `_2` so I won't either. Moving on?

```haskell
choosing l1 l2 fn source =
  case source of
    Left s1  -> Left <$> l1 fn s1
    Right s2 -> Right <$> l2 fn s2
```

This is the solution to `choosing`. It has an obvious implementation which follows from the third sphere and is left as an exercise to the reader. Just kidding. Took me a while. Sometimes I feel like I need to zone out and squint to grok Haskell. If I stare too hard and focus on only a tiny part of a function the bigger picture often eludes me. Or something like that? Also expand type synonyms, specialize types and write down your intermediate steps. Otherwise you'll constantly lose track and have to start again.

In this case the expanded signature is long. Really long. But by this time I felt like I was starting to maybe get a hang of at least some things. A lens is a modifier function and a source. (That's what the author was getting at with the whole setter is the getter thing I think). So when you see Lens, ignore the types for a moment and just think of a function, that takes a modifier and a value. That's a lens. It's a function with 2 arguments.

In this case, the value (called source in my snippet) can be `Left` or `Right`. So we start by checking which one it is. If you don't know how to get started on this one, I'd say always start with the `source`. And because `choosing` _is a Lens_ and a Lens is a function taking _two arguments_ we just start our implementation by checking its second argument, the source, from where everything else follows eventually.

Honestly I don't know if the above is correct. It's so simple and helps me so much that there's no way the lens documentation doesn't mention this in font size 200 on the front page if it were true. Although looking at how `Lens` is [defined in the actual package](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Type.html#t:Lens) it seems true enough. Yet they say that

> Lens' bigger smaller

and

`type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)`

or simpler

`type Lens' a b = (b -> f b) -> (a -> f a)`

The way the parentheses are placed here is really confusing. It looks like a `Lens'` is just a function taking two arguments without ever returning anythin. Of course the parentheses are also irrelevant and so it does return an `f a`. But that's somewhat like what I ranted about in "Naming", that the way things look definitely affects my understanding since I often judge a cover by a book, so to speak, since I am not yet experienced enough to immediately look past that cover.

I would say:

> What is a lens?

> A lens is a function taking two arguments: a modifier function, and a source value.

The word "modifier" is probably wrong and misses something but whatever.

For this snippet

```haskell
choosing l1 l2 fn source =
  case source of
    Left s1  -> Left <$> l1 fn s1
    Right s2 -> Right <$> l2 fn s2
```

we pass the modifier function to the first or second lens (depending on the `Either`). Now that lens was applied to its first argument, and the only thing left to do (in fact that's the only thing you can do), is apply it to its second argument - the source. We then get back an `f a`. **But the return type of Lens is `f target`**. The type signature of `choosing` is

```haskell
choosing ::
     Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
--                                                            ^^^^^ target

type Lens s t a b
   = forall f. Functor f =>
                 (a -> f b) -> s -> f t

-- so specialized

type SpecializedLens
   forall f. Functor f =>
                (a -> f b) -> (Either s1 s2) -> f (Either t1 t2)
```

Because of the return type `f (Either t1 t2)` applying `l2 fn` to `s2` is not enough. That's merely running a single Lens which gives a functor one level deep, but we need two levels. `Left <$> l1 fn s1` does that. GG.

## All is Vain

Wow. The next one made me question pursuing Haskell any further. I had to check Github, realized that I didn't understand the solutions with `liftA2` and I didn't even try to understand `&&&`. That meant I had to actually solve it and I needed to do it with as simple as code as possible. So pretty much the opposite of the bonus points.

```haskell
-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = _
```

Frankly I didn't know where to start. `a -> b` is not compatible with `a -> f b`. That lead me off on a tangent to find a function that helps with that until an SO post mentioned the obvious: `a -> f a` is a type constructor. Like `\x -> Just x`. The function is supposed to return a tuple so the functor we'll be using is `(,)` (which has a functor instance).

That was at least one step forward.

I also knew that I somehow needed to take the function just called `f` in the snippet _taken from the tutorial_ and apply the lens `l` to that function and the source `s`. I need to do `l f' s`, and somehow turn the given `f` into my `f'`.

A lens takes an `a -> f b` but I only had `a -> b`. I've read and written enough Haskell to know that this smells like `lift`. But e.g., `liftA2` makes an unlifted function `a -> b -> c` work on `f a` and `f b` and `f c`. I however had an `a` as an input but needed an `f something` as an output. And I also know that `a -> f a` is simply a constructor. Something something `(,)` and `lift`?

I took the low road and reverse engineered the `liftA2` solutions out there. I arrived at

```haskell
(<%~) lens fn source = lens modifierFn source
  where
    modifierFn x = ((,) . fn) x (fn x)
```

This is nice for people like me (or just me). It's very clear how this leads to `(b,t)`. It's very explicity and verbose. So how do I get from this to understand the `liftA2` solutions? I looked into the functor chapter from Haskell from First Principle again and immediately realized that I need to go through the entire book again (I skimmed the chapters one strictess and exceptions and I apparently never learned or forgot a lot of other stuff). More importantly though, I looked into the **functor instance for functions**.

Recap: I have an `a -> b` and the `b` needs to be put into a functor (here a tuple). I can achieve that by `fmap`ping the functor over the function. The functor instance for functions is actually just composition. So `fmap (,) fn` is the same as `(,) . fn` which is exactly what I did in the above snippet. Let's see what the snippet looks like, if we extract the `(,) . fn` part into its own function.

```haskell
(<%~) lens fn source = lens modifierFn source
  where
    modifierFn x = helper x (fn x)
    helper = (,) . fn
```

That looked like something rather common and after looking at several different resources I eventually came upon the definition of `<*>` for functions.

```haskell
instance Applicative ((->) r) where
    f <*> g = \x -> f x (g x)
```

What a surprise. The `modifierFn` I ended up with after extracting `helper` looks exactly like `<*>`. So

```haskell

(<%~) lens fn source = lens modifierFn source
  where
    -- vvv--- same as --->>> modifierFn x = helper x (fn x)
    modifierFn = helper <*> fn
    helper = (,) . fn
```

I then looked at the source for `liftA2` again, which is `liftA2 f x = (<*>) (fmap f x)`. The last argument was eta reduced away, so it's actually (with some renaming) `liftA2 f fa fb = (<*>) (fmap f fa) fb`. So... let's take the `liftA2` solution and compare it to what I've now learned (or relearned).

```haskell
(<%~) lens fn = lens (liftA2 (,) fn fn)
-- ^                          f  fa fb <-- in the above paragraph
-- Expanded
(<%~) lens fn = lens $ (<*>) (fmap (,) fn) fn
(<%~) lens fn = lens $ (<*>) ((,) . fn) fn
(<%~) lens fn = lens $ \x -> ((,) . fn) x (fn x)
```

And there you have it. At the bottom is my verbose approach, and then we "just" extract the patterns for which Haskell already has typeclasses.

# Recap

My fundamental problem with the exercises was that I was looking for a clever solution right from the start. But I don't know enough about Haskell, nor anything about category theory, to make the mental leap from type signature to implementation that does it all in a one-liner using existing abstractions.

What I should have done in all cases is to just try to write a verbose, manual solution and then see if it can be reduced further. The author of "Lens over Tea" even offers the following solutions:

> But the right thing to do is one of these:

```haskell
(<%~) l f = l (\x -> let fx = f x in (fx, fx))

(<%~) l f = l $ (\t -> (t, t)) . f
```

That's really easy to understand (and only computes `f x` once). But I don't know if this is considered idiomatic Haskell. And that's one of my major issues with this language right now. I have no idea what kind of clever is too clever and what kind of verbose is too verbose.
