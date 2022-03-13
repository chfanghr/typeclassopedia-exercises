# Exercises from [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)

## Functor

### Instances

1. Implement `Functor` instances for `Either e` and `((->) e)`.

```haskell
data Either l r = Left l | Right r

instance Functor (Either l) where
  fmap f (Right x) = Right $ f x
  fmap _ (Left x) = Left x

newtype Arrow a b = Arrow (a -> b) -- (->) a b

-- TODO: How can I hide (->) from Base???

instance Functor (Arrow a) where
  -- fmap :: (b -> c) -> Arrow a b -> Arrow a c
  fmap f (Arrow g) = Arrow (f . g)
```

2. Implement `Functor` instances for `((,) e)` and for `Pair`, defined as

```haskell
data Pair a = Pair a a
```

```haskell
newtype Tuple a b = Tuple (a, b)

instance Functor (Tuple a) where
  fmap f (Tuple (x, y)) = Tuple (x, f y)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
```

3. Implement a `Functor` instance for the type `ITree`, defined as

```haskell
data ITree a
  = ILeaf (Int -> a)
  | INode [ITree a]
```

```haskell
instance Functor ITree where
  fmap f (ILeaf g) = ILeaf (f . g)
  fmap f (INode ts) = INode $ map (fmap f) ts
```

4. Give an example of a type of kind `* -> *` which cannot be made an instance of `Functor` (without using `undefined`).

Consider:

```haskell
data G a = G (a -> Int)

  instance Functor G where
    fmap :: (a -> b) -> G a -> G b
    fmap f (G g) = ... -- Here g has type a -> Int, while f has type a -> b
```

We want to compose a function which has type `b -> Int`, using `f` and `g `, but there is no *resonable* way to implement this.

5. Is this statement true or false?

> The composition of two `Functors` is also a` Functor`.

   If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code.

```haskell
newtype Compose a b s = Compose (a (b s))

{-
Functor for (a s1) is:
  fmap :: (s1 -> s2) -> a s1 -> a s2
Here sn is (b sn'), so new functor for type a(b s) is:
  fmap :: (b s1' -> b s2') -> a (b s1') -> a (b s2')
While functor for (b sn') is:
  fmap :: (s1' -> s2') -> b s1' -> b s2'
-}

instance (Functor a, Functor b) => Functor (Compose a b) where
  fmap f (Compose ctx) =
    -- Apply transfermer to inner context b first.
    -- Type of f' is (b s1' -> b s2'),
    --  and that's exactly what we need for the second transform.
    let f' = fmap f
     in Compose $ fmap f' ctx
```

### Laws

1. Although it is not possible for a `Functor` instance to satisfy the first `Functor` law but not the second (excluding `undefined`), the reverse is possible. Give an example of a (bogus) `Functor` instance which satisfies the second law but not the first.

TODO: idk for now :(

2. Which laws are violated by the evil `Functor` instance for list shown above: both laws, or the first law alone? Give specific counterexamples.

```haskell
-- Said "evil" Functor instance:
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap g (x:xs) = g x : g x : fmap g xs
```

Consider xs="abc", then:

```
(fmap id) . (fmap id) $ xs = "aaaabbbbcccc"
fmap (id . id) xs = "aabbcc"
```

They should be identical but they're not.

Conclusion: It violates the second law as well

## Applicative

### Laws

1. (Tricky) One might imagine a variant of the interchange law that says something about applying a pure function to an effectful argument. Using the above laws, prove that

   ```
   pure f <*> x = pure (flip ($)) <*> x <*> pure f
   ```

```
flip f b a = f a b
flip ($) x f = ($) f x = f x
($ f) = \x -> x f

pure (flip ($)) <*> x <*> pure f
= (pure (flip ($)) <*> x) <*> pure f
= (pure (\x' f' -> flip ($) x f') <*> x) <*> pure f
= (pure (\x' f' -> f' x') <*> x) <*> pure f
= pure ($ f) <*> (pure (\x' f' -> f' x') <*> x)
= pure(.) <*> pure ($ f) <*> pure (\x' f' -> f' x') <*> x
= pure ((\x -> x f) . (\x' f' -> f' x')) <*> x
= pure f <*> x
```

### Instances

1. Implement an instance of `Applicative` for `Maybe`.

```haskell
data Maybe a = Nothing | Just a

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Applicative Maybe where
  pure x = Just x

  Nothing <*> x = Nothing
  (Just f) <*> x = fmap f x
```

2. Determine the correct definition of `pure` for the `ZipList` instance of `Applicative`—there is only one implementation that satisfies the law relating `pure` and `(<*>)`.

```haskell
newtype ZipList a = ZipList {getZipList :: [a]}

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ map f xs

instance Applicative ZipList where
  pure f = ZipList $ repeat f
  (ZipList fs) <*> (ZipList xs) = ZipList $ zipWith ($) fs xs
```

### Utility functions

1. Implement a function

   ```
   sequenceAL :: Applicative f => [f a] -> f [a]
   ```

   There is a generalized version of this, `sequenceA`, which works for any `Traversable` (see the later section on Traversable), but implementing this version specialized to lists is a good exercise.

```haskell
-- 1. f a -> f [a]
-- 2. [f[a]] -> f[a]

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL xs = h
  where
    g = map ((: []) <$>) xs
    h = foldl (\l x -> (++) <$> l <*> x) (pure []) g
```

### Alternative formulation

```haskell
class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f(a -> b) -> f a -> f b
```

1. Implement `pure` and `(<*>)` in terms of `unit` and `(**)`, and vice versa.

```haskell
pure x = x <$ unit
f <*> x = uncurry ($) <$> (f ** x)
```

```haskell
unit = pure ()
x ** y = (,) <$> x <$> y
```

2. Are there any `Applicative` instances for which there are also functions `f () -> ()` and `f (a,b) -> (f a, f b)`, satisfying some "reasonable" laws?

`Identity`

TODO: implementation

3. (Tricky) Prove that given your implementations from the first exercise, the usual `Applicative` laws and the `Monoidal` laws stated above are equivalent.

TODO: idk 

