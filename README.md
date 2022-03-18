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

> Precisely, the exact issue here is that the variable we wish to manipulate is in a negative position, and thus we can't map it forward.

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

```haskell
data Illegal a = A | B deriving (Eq)
instance Functor Illegal where
         fmap _ _ = B
```
```
first law: 
fmap id A = B
fmap id B = B -- ah shit

second law:
fmap (id.id) = (fmap id ) . (fmap id) -- pass
```

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

sequenceAL' :: Applicative f => [f a] -> f [a]
sequenceAL' [] = pure []
sequenceAL' (x:xs) = (:) <$> x <*> sequenceAL1 xs
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
x ** y = (,) <$> x <*> y
```

2. Are there any `Applicative` instances for which there are also functions `f () -> ()` and `f (a,b) -> (f a, f b)`, satisfying some "reasonable" laws?

```haskell
f1 :: Identity () -> ()
f1 _ = () -- unit <-> () 

f2 :: Identity (a, b) -> (Identity a, Identity b)
f2 (Identity (x, y)) = (Identity x, Identity y) -- f (a, b) <-> f a ** f b 
```

3. (Tricky) Prove that given your implementations from the first exercise, the usual `Applicative` laws and the `Monoidal` laws stated above are equivalent.

```
unit ** v = pure () ** v
					= pure (,) <*> pure () <*> v
					= pure ((), ) <*> v
					≅ pure id <*> v
					= v 

u ** unit = u ** pure ()
					= pure (,) <*> u <*> pure ()
					= pure ($ ()) <*> (pure (,) <*> u)
					= pure (.) <*> pure ($ ()) <*> pure (,) <*> u
					= pure (($ ()) . (,)) <*> u
					= pure (\x -> (x , ())) <*> u
					≅ pure id <*> u 
					= u
					
-- u ** (v ** w) ≅ (u ** v) ** w

(u ** v) ** w = pure (,) <*> (pure (,) <*> u <*> v) <*> w
							= pure (.) <*> pure(,) <*> pure(,) <*> u <*> v <*> w
							= pure ((,) . (,)) <*> u <*> v <*> w -- left hand side

u ** (v ** w) = pure (,) <*> u <*> (v ** w)
							= pure (,) <*> u <*> (pure (,) <*> v <*> w)
							= pure (.) <*> pure (,) <*> u <*> pure (,) <*> v <*> w
							= pure ((.) (,)) <*> u <*> pure (,) <*> v <*> w
							= pure ($ (,)) <*> pure ((.) (,)) <*> u <*> v <*> w
							= pure ((,) . (,)) <*> u <*> v <*> w -- right hand side
							
The left hand side is identical to the right hand side.
```

## Monad

### Instances

1. Implement a `Monad` instance for the list constructor, `[]`. Follow the types!

```haskell
data List a = Empty | Cons a (List a)

concat' :: List a -> List a -> List a
concat' xs Empty = xs
concat' Empty xs = xs
concat' (Cons x xs') xs = Cons x (concat' xs' xs)

instance Functor List where
  fmap _ Empty = Empty
  fmap f (Con x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Empty
  Empty <*> _ = Empty
  (Con f fs) <*> xs = concat' (fmap f xs) (fs <*> xs)

instance Monad List where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  -- [] >>= _ = []
  Empty >>= _ = Empty
  -- (x:xs) >>= f = (f x) ++ (xs >>= f)
  (Cons x xs) >>= f = concat' (f x) (xs >>= f)
```

2. Implement a `Monad` instance for `((->) e)`.

```haskell
instance Applicative (Arrow a) where
  -- pure :: b -> Arrow a b
  -- pure :: b -> ((->) a b)
  pure x = Arrow $ const x

  -- (<*>) :: Arrow a (b -> c) -> Arrow a b -> Arrow a c
  -- (<*>) :: ((->)a (b -> c)) -> ((->)a b) -> ((->)a c)
  -- f :: a -> (b -> c)
  -- g :: a -> b
  -- r :: a -> c
  (Arrow f) <*> (Arrow g) = Arrow $ \x -> f x $ g x

instance Monad (Arrow a) where
  -- (>>=) :: Arrow a b -> (b -> Arrow a c) -> Arrow a c
  (Arrow g) >>= f = Arrow $
    \x ->
      let (Arrow h) = f $ g x
       in h x
```

3. Implement `Functor` and `Monad` instances for `Free f`, defined as 
   
   ``` haskell
   data Free f a = Var a
                 | Node (f (Free f a))
   ```
   
   You may assume that `f` has a `Functor` instance. This is known as the *free monad* built from the functor `f`.

```haskell
instance (Functor f) => Functor (Free f) where
  -- fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Var a) = Var $ f a
  fmap f (Node nested) = Node $ fmap (fmap f) nested

instance (Functor f) => Applicative (Free f) where
  -- pure :: a -> Free f a
  pure x = Var x

  -- (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  (Var g) <*> (Var x) = Var $ g x
  (Var g) <*> (Node x) = Node $ fmap (fmap g) x
  (Node g) <*> x = Node $ fmap (<*> x) g

instance (Functor f) => Monad (Free f) where
  -- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (Var x) >>= f = f x
  (Node x) >>= f = Node $ fmap (>>= f) x
```

### Intuition

1. Implement `(>>=)` in terms of `fmap` (or `liftM`) and `join`.

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
fmap = undefined

join :: f (f a) -> f a
join = undefined

(>>=) :: f a -> (a -> f b) -> f b
x >>= f =
  -- x :: f a
  -- f :: (a -> f b)
  let nested = fmap f x in -- nested :: f (f b)
    join nested -- remove one layer of context
```

2. Now implement `join` and `fmap` (`liftM`) in terms of `(>>=)` and `return`.

```haskell
join :: (Monad m) => m (m a) -> m a
join x = x >>= id

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f x = x >>= return . f
```

### Laws

1. Given the definition `g >=> h = \x -> g x >>= h`, prove the equivalence of the above laws and the usual monad laws.

```
return >=> g = \x -> return x >>= g = g = \x -> g x
So: return x >>= g = g x

g >=> return = \x -> g x >>= return = g = \x -> g x
So: g >>= return = g

(g >=> h) >=> k = g >=> (h >=> k)
\x -> ((g >=> h) x) >>= k = \x -> (g x) >>= (h >=> k)
\x -> ((\y -> g y >>= h) x) >>= k = \x -> (g x) >>= (\y -> h y >>= k)
\x -> (g x >>= h) >>= k = \x -> (g x) >>= (\y -> h y >>= k)
let m = g x, we have:
\x -> (m >>= h) >>= k = \x -> m >>= (\y -> h y >>= k)
So: (m >>= h) >>= k = m >>= (\x -> h x >>= k)
```



## Monad transformers

### Composing monads

1. Implement `join :: M (N (M (N a))) -> M (N a)`, given `distrib :: N (M a) -> M (N a)` and assuming `M` and `N` are instances of `Monad`

```haskell
distrib :: (Monad m, Monad n) => n (m a) -> m (n a)
distrib = undefined

join :: (Monad m, Monad n) => m (n (m (n a))) -> m (n a)
join x =
  let x''' = -- x''' :: m(n a)
        x
          >>= ( \x -> -- x :: n(m(n a))
                  let x' = distrib x -- x' :: m(n(n a))
                   in let x'' = join <$> x' -- x'' :: m(n a)
                       in x''
              )
   in x'''
```

## Foldable

### Instances and examples

1. Implement `fold` in terms of `foldMap`.

```Haskell
fold :: (Monoid m, Foldable t) => t m -> m
fold = foldMap id
```

2. What would you need in order to implement `foldMap` in terms of `fold`?

```Haskell
-- Have to convert t a to t b first

foldMap :: (Foldable t, Functor t, Monoid m) => (a -> m) -> t a -> m
foldMap f = fold . fmap f
```

3. Implement `foldMap` in terms of `foldr`.

```Haskell
foldr :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldr = undefined

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (\x l -> l <> f x) mempty
```

4. Implement `foldr` in terms of `foldMap` (hint: use the `Endo` monoid).

```Haskell
newtype Endo a = Endo {appEndo :: a -> a}

instance Semigroup (Endo a) where
  (Endo x) <> (Endo y) = Endo (x . y)

instance Monoid (Endo a) where
  mempty = Endo id

foldr :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldr f x t = let g = foldMap (Endo . f) t in appEndo g x
```

5. What is the type of `foldMap . foldMap`? Or `foldMap . foldMap . foldMap`, etc.? What do they do?

```
foldMap :: (Monoid, Foldable t) m => (a -> m) -> t a -> m

(I confess that I used ghci cause I am so lazy :)))

f :: (Monoid m, Foldable t1, Foldable t2) => (a -> m) -> t1 (t2 a) -> m
f = foldMap . foldMap

f' :: (Monoid m, Foldable t1, Foldable t2, Foldable t3) => (a -> m) -> t3 (t1 (t2 a)) -> m
f' = foldMap . foldMap . foldMap

more t*....
```

Convert a n-dimensional nested structure to a single value.

### Derived folds

1. Implement `toList :: Foldable f => f a -> [a]` in terms of either `foldr` or `foldMap`.

```Haskell 
toList :: Foldable f => f a -> [a]
toList = foldMap (: [])
```

2. Show how one could implement the generic version of `foldr` in terms of `toList`, assuming we had only the list-specific `foldr :: (a -> b -> b) -> b -> [a] -> b`.

```Haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = undefined

foldrg :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldrg f i t = foldr f i $ toList t
```

3. Pick some of the following functions to implement: `concat`, `concatMap`, `and`, `or`, `any`, `all`, `sum`, `product`, `maximum`(`By`), `minimum`(`By`), `elem`, `notElem`, and `find`. Figure out how they generalize to `Foldable` and come up with elegant implementations using `fold` or `foldMap` along with appropriate `Monoid` instances.

```Haskell
concat :: Foldable t => t [a] -> [a]
concat = fold

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f = foldMap f

and :: Foldable t => t Bool -> Bool
and t = appEndo (foldMap (Endo . (&&)) t) True

or :: Foldable t => t Bool -> Bool
or t = appEndo (foldMap (Endo . (||)) t) False

all :: Foldable t => (a -> Bool) -> t a -> Bool
all pred = and1 . foldMap (\x -> [pred x])

any :: Foldable t => (a -> Bool) -> t a -> Bool
any pred = or1 . foldMap (\x -> [pred x])

newtype Product a = Product {getProduct :: a}

instance (Num a) => Semigroup (Product a) where
  (Product x) <> (Product y) = Product $ x * y

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

newtype Sum a = Sum {getSum :: a}

instance (Num a) => Semigroup (Sum a) where
  (Sum x) <> (Sum y) = Sum $ x + y

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

sum :: (Num a, Foldable t) => t a -> a
sum = getSum . foldMap Sum

product :: (Num a, Foldable t) => t a -> a
product = getProduct . foldMap Product

data Maximum a = Maximum {getMaximum :: a} | Min -- Get rid of Bounded

instance (Ord a) => Semigroup (Maximum a) where
  Min <> x = x
  x <> Min = x
  (Maximum x) <> (Maximum y) = Maximum $ max x y

instance (Ord a) => Monoid (Maximum a) where
  mempty = Min

data Minimum a = Minimum {getMinimum :: a} | Max

instance (Ord a) => Semigroup (Minimum a) where
  Max <> x = x
  x <> Max = x
  (Minimum x) <> (Minimum y) = Minimum $ min x y

instance (Ord a) => Monoid (Minimum a) where
  mempty = Max

minimum :: (Foldable t, Ord a) => t a -> a
minimum = getMinimum . foldMap Minimum

maximum :: (Foldable t, Ord a) => t a -> a
maximum = getMaximum . foldMap Maximum

elem :: (Eq a, Foldable t) => a -> t a -> Bool
elem y = any (== y)

notElem :: (Eq a, Foldable t) => a -> t a -> Bool
notElem y = all (/= y)

data First a = First a | FEmpty

getFirst :: First a -> Maybe a
getFirst FEmpty = Nothing
getFirst (First x) = Just x

instance Semigroup (First a) where
  FEmpty <> x = x
  x <> _ = x

instance Monoid (First a) where
  mempty = FEmpty

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find pred = getFirst . foldMap (\x -> if pred x then First x else FEmpty)

data MaximumBy a = MaximumBy (a -> a -> Ordering) a | Min'

getMaximum' :: MaximumBy a -> a
getMaximum' (MaximumBy _ x) = x
getMaximum' _ = error "what the heck man"

instance Semigroup (MaximumBy a) where
  x <> Min' = x
  Min' <> x = x
  (MaximumBy f x) <> (MaximumBy _ y) = MaximumBy f $ if f x y == GT then x else y

instance Monoid (MaximumBy a) where
  mempty = Min'

data MinimumBy a = MinimumBy (a -> a -> Ordering) a | Max'

getMinimum' :: MinimumBy a -> a
getMinimum' (MinimumBy _ x) = x
getMinimum' _ = error "what the heck man"

instance Semigroup (MinimumBy a) where
  x <> Max' = x
  Max' <> x = x
  (MinimumBy f x) <> (MinimumBy _ y) = MinimumBy f $ if f x y == LT then x else y

instance Monoid (MinimumBy a) where
  mempty = Max'

maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy f = getMaximum' . foldMap (MaximumBy f)

minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy f = getMinimum' . foldMap (MinimumBy f)
```

## Traversable

### Intuition

1. There are at least two natural ways to turn a tree of lists into a list of trees. What are they, and why?
2. Give a natural way to turn a list of trees into a tree of lists.
```Haskell
data BinaryTree a = BNode (BinaryTree a) a (BinaryTree a) | BLeaf deriving (Show)

instance Functor BinaryTree where
  fmap _ BLeaf = BLeaf
  fmap f (BNode l x r) = BNode (fmap f l) (f x) (fmap f r)

instance Foldable BinaryTree where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> Foldable a -> m
  foldMap _ BLeaf = mempty
  foldMap f (BNode l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable BinaryTree where
  -- sequenceA :: (Applicative f) => BinaryTree (f a) -> f (BinaryTree a)
  sequenceA BLeaf = pure BLeaf
  sequenceA (BNode l x r) = BNode <$> sequenceA l <*> x <*> sequenceA r

tree :: BinaryTree [Int]
tree = BNode node [1, 5] node
  where
    node = BNode (node' [2]) [3] (node' [4])
    node' x = BNode BLeaf x BLeaf

{-
              [2]
        [3]
              [4]
[1, 5]
              [2]
        [3]
              [4]
-}

--

{-
[BNode (BNode (BNode BLeaf 2 BLeaf) 3 (BNode BLeaf 4 BLeaf)) 1 (BNode (BNode BLeaf 2 BLeaf) 3 (BNode BLeaf 4 BLeaf)),
 BNode (BNode (BNode BLeaf 2 BLeaf) 3 (BNode BLeaf 4 BLeaf)) 5 (BNode (BNode BLeaf 2 BLeaf) 3 (BNode BLeaf 4 BLeaf))]
-}
treeOfListsToListOfTrees1 :: BinaryTree [a] -> [BinaryTree a]
treeOfListsToListOfTrees1 = sequenceA

{-
[BNode BLeaf 2 BLeaf,BNode BLeaf 3 BLeaf,BNode BLeaf 4 BLeaf,
 BNode BLeaf 1 BLeaf,
 BNode BLeaf 5 BLeaf,
 BNode BLeaf 2 BLeaf,BNode BLeaf 3 BLeaf,BNode BLeaf 4 BLeaf]
-}
treeOfListsToListOfTrees2 :: BinaryTree [a] -> [BinaryTree a]
treeOfListsToListOfTrees2 = foldMap (fmap (\x -> BNode BLeaf x BLeaf))

-- 

listOfTreesToTreeOfLists :: [BinaryTree a] -> BinaryTree [a]
listOfTreesToTreeOfLists ts = BNode BLeaf (mconcat $ map toList ts) BLeaf
```
3. What is the type of `traverse . traverse`? What does it do?
```
(.) :: (b -> c) -> (a -> b) -> a -> c
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

((t a -> f (t b)) -> t (t a) -> f (t (t b))) -> 
  ((a -> f b) -> t a -> f (t b)) -> 
  (a -> f b) -> f (t (t b))
```
``` Haskell
f :: (Applicative f, Traversable t1, Traversable t2) => (a -> f b) -> t1 (t2 a) -> f (t1 (t2 b))
f = traverse . traverse
```
Traverse a nested set of two traversable structures.

4. Implement `traverse` in terms of `sequenceA`, and vice versa.
```Haskell
traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse f t = sequenceA $ f <$> t

sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA = traverse id
```

###  Instances and examples

1. implement `fmap` and `foldMap` using only the `Traversable` methods. (Note that the `Traversable` module provides these implementations as `fmapDefault` and `foldMapDefault`.)

```haskell
newtype Identity a = Identity {getIdentity :: a}

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure x = Identity x

  (Identity f) <*> (Identity x) = Identity $ f x

fmap' :: (Traversable t) => (a -> b) -> t a -> t b
fmap' f t = getIdentity $ traverse (Identity . f) t
  
-- My brain hurts
-- Spent 1 hr wrting 10 loc

newtype MonoidConst a b = MonoidConst {getConst :: a}

instance Functor (MonoidConst a) where
  fmap _ (MonoidConst x) = MonoidConst x

instance (Monoid a) => Applicative (MonoidConst a) where 
  -- pure :: b - > MonoidConst a b 
  pure x = MonoidConst mempty

  -- (<*>) :: MonoidConst a (b -> c) -> Monoid a b -> Monoid a c
  (MonoidConst x) <*> (MonoidConst y) = MonoidConst $ x <> y 

foldMap' :: (Monoid m, Traversable t) => (a -> m) -> t a -> m
foldMap' f t = getConst $ traverse (MonoidConst . f) t
```

2. Implement `Traversable` instances for `[]`, `Maybe`, `((,) e)`, and `Either e`

```haskell
{-# LANGUAGE TupleSections #-}

-- ......

instance Foldable (Either l) where
  -- foldMap :: Monoid m => (a -> m) -> Either l a -> m
  foldMap _ (Left e) = mempty
  foldMap f (Right x) = f x

instance Traversable (Either l) where
  -- traverse :: Applicative f => (a -> f b) -> Either l a -> f (Either l b)
  traverse _ (Left e) = pure $ Left e
  traverse f (Right x) = Right <$> f x

-- or:
-- sequenceA :: (Applicative f) => (Either l (f a)) -> (f (Either l a))
-- sequenceA (Left e) = pure $ Left e
-- sequenceA (Right x) = Right <$> x

instance Foldable (Tuple a) where
  -- foldMap :: Monoid m => (b -> m) -> Tuple a b -> m
  foldMap f (Tuple (_, x)) = f x

instance Traversable (Tuple a) where
  -- sequenceA :: Applicative f => Tuple a (f b) -> f (Tuple a b)
  sequenceA (Tuple (y, x)) = Tuple . (y,) <$> x

instance Foldable List where
  -- foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap _ Empty = mempty
  foldMap f (Con x xs) = f x <> foldMap f xs

instance Traversable List where
  -- sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Empty = pure Empty
  sequenceA (Con x xs) = Con <$> x <*> sequenceA xs

instance Foldable Maybe where
  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x

instance Traversable Maybe where
  -- sequenceA :: Applicative f => Maybe (f a) -> f (Maybe a)
  sequenceA (Just x) = Just <$> x
  sequenceA Nothing = pure Nothing

```

3. Explain why `Set` is `Foldable` but not `Traversable`.

`Set` is not a `Functor` instance. 

(`map` function for `Set` has type `map :: Ord b => (a -> b) -> Set a -> Set b`. So the type parameter b needs to be `Ord` as well as a, which make  it impossible for `Set` to be a `Functor`.) 

4. Show that `Traversable` functors compose: that is, implement an instance for `Traversable (Compose f g)` given `Traversable` instances for `f` and `g`.

```haskell
instance (Foldable a, Foldable b) => Foldable (Compose a b) where
  -- foldMap :: Monoid m => (s -> m) -> Compose a b s -> m
  foldMap f (Compose ctx) = foldMap (foldMap f) ctx

instance (Traversable a, Traversable b) => Traversable (Compose a b) where
  -- sequenceA :: Applicative f => Compose a b (f s) -> f (Compose a b s)
  sequenceA (Compose ctx) = Compose <$> h
    where 
      g = fmap sequenceA ctx
      h = sequenceA g
```

## Monad transformers

### Definition and laws

1. What is the kind of `t` in the declaration of `MonadTrans`?

```
(* -> *) -> * -> *
```

### Composing monads

- Implement `join :: M (N (M (N a))) -> M (N a)`, given `distrib :: N (M a) -> M (N a)` and assuming `M` and `N` are instances of `Monad`.

```haskell
distrib :: (Monad m, Monad n) => n (m a) -> m (n a)
distrib = undefined

join' :: m (n (m (n a))) -> m (n a)
join' x =
  let x''' = -- x''' :: m(n a)
        x
          >>= ( \x -> -- x :: n(m(n a))
                  let x' = distrib x -- x' :: m(n(n a))
                   in let x'' = join <$> x' -- x'' :: m(n a)
                       in x''
              )
   in x'''
```

(I finally remembered that I can use a `do` block)

```haskell
join'' :: (Monad m, Monad n) => m (n (m (n a))) -> m (n a)
join'' x = do
  nmna <- x
  nna <- distrib nmna
  let na = join nna
  return na
```



