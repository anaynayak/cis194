# Haskell cis 194

* Functional
  * Functions as first class
  * evaluating expressions instead of executing instructions
* Statically typed
  * Compile time safety
* Pure
  * Virtue
    * Immutable
    * Return same value for given args
    * Side effect free
  * Why:
    * Parallelise
    * Reason easily, debug etc
    * Equational reasoning and refactoring
* Lazy
  * Expressions not evaluated until required
  * infinite data structures
  * Enables compositional programming [?]
  * Difficult to reason w.r.t. space time usage


### Course theme:
* Typed
  * Documentation
  * Compile time safety
  * Thinking
* Wholemeal programming
  * Think big. Develop solution space.
  * Solve general problem extract specific bits by transforming general program into more specialized ones.
* Abstraction
  * DRY


### Enumeration types

`data Thing = Shoe | Ship (deriving Show)`

### Algebraic data types

`data FailableDouble = Failure | OK Double (deriving Show)`

`Failure` and `OK` are data constructors. `OK` takes a param
`FailableDouble` is a type constructor.

### Pattern matching

```haskell
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", your favorite thing is lame."
```

`case` statement.

```haskell
case "hello" of
  [] -> 3
  ('H':s) -> 10
  _ -> 1
```

### Recursive data type

`data IntList = Empty | Cons Int IntList`

### Recursion patterns

* Map
* Filter
* Fold

### Polymorphic type:

`data List t = E | C t (List t)`

t is a type variable

### Polymorphic functions

`mapList :: (a -> b) -> List a -> List b`

With polymorphic functions caller gets to pick types.

### Total v/s partial functions

e.g. head is a partial function. crashes with an empty list.

Use the `safe` package.

### Higher order programming

Anonymous function or lambda abstraction

`(\x -> x > 100)`

Operator section: if ? is an operator, then (?y) is equivalent to the function \x -> x ? y

`.` functional composition

function arrows associate to the right, that is, `W -> X -> Y -> Z` is equivalent to `W -> (X -> (Y -> Z))`

Function application, in turn, is left-associative. That is, `f 3 2` is really shorthand for `(f 3) 2`

```haskell
comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

comp f g = f . g
```

The arguments should be ordered from from “least to greatest variation”,

Currying - Represent multi argument functions as one-argument functions.
Alternatively, can pass a single paarameter to a function as a tuple. `uncurry` can be used to unwrap a tuple into args.

### Eta conversion

eta reduction `\x -> abs x` to  `abs`
eta abstraction is the reverse

### Beta reduction - application of a function to an expression

### Alpha conversion - same function different names.

### Fold

> [Fold](https://wiki.haskell.org/Fold)

```haskell
foldr f z [a,b,c] == a `f` (b `f` (c `f` z))
foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c
```

e.g.

Hangs:
`take 20 $ foldl (\acc s -> acc ++ [s]) [] [1..]`
Runs to compleltion:
`take 20 $ foldr (:) [] [1..]`
Stack overflow:
foldr max 0 [1..]

Reducible expression redex . `(+)` is not redex. `max` [?]

GHC has a lazy reduction strategy. Use foldl' . See `seq` which reduces first param before going for the rest.

### Polymorphisms and type classes

Caller gets to choose type. What are other examples ? [?]

We say that a function like f :: a -> a -> a is parametric in the type a . Parametricity corresponds to guarantees not restrictions

Examples of parameteric types:

a -> a (id)
a -> b (map)
a -> b -> a (const)
[a] -> [a] (list functions)
(b -> c) -> (a -> b) -> (a -> c) (.)
(a -> a) -> a -> a ($)

(+) :: Num a => a -> a -> a

### Type classes

Num, Eq, Ord, and Show are type classes, and we say that (==), (<), and (+) are “type-class polymorphic”. Intuitively, type classes correspond to sets of types which have certain operations defined for them, and type class polymorphic functions work only for types which are instances of the type class(es) in question.

Type classes are different from java interfaces. Type class instances are declared separately from the type classes. In another module as well. [?]

Multiple-dispatch : not possible in java. Depends on types of both a, b below

```haskell
class Blerg a b where
  blerg :: a -> b -> Bool
```


type class constraints can be on the instance as well as functions.

```haskell
instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y
```

[?] Language extensions FlexibleInstances etc

When using `newtype`, you're restricted to just one constructor with one field.


### Lazy evaluation

For Java, params are evaluated regardless of whether the method actually consumes the passed variables. Side-effect is a primary reason why strict param evaluation is needed.

For lazy, evaluation is delayed as long as possible. Unevaluated expressions are called as _thunk_

A trigger for evaluation could for e.g. be a pattern match. A `Maybe` need not be evaluated if the method doesnt care of the _shape_ of it. However if we pattern match to `Nothing` or `Just` then we need to know. The thunks are evaluated _just_ _enough_.

GHC uses _graph_ _reduction_ . Expression represented as graph so that same expressions can be pointers and evaluated only once.

```haskell
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
```

### Lazy / short-circuit

```haskell
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False
```

* Infinite data structures are effectively just thunks.
* With wholemeal programming, only those thunks that are needed are consumed.
* Can define our own control structures
* Dynamic programming [?]


### Folds with monoids

```haskell
exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)
```

####  fold-map fusion
[fold-map fusion](http://www.cantab.net/users/antoni.diller/haskell/units/unit06.html) lets you replace such a definition by one that only involves foldr:

foldr op u . map f = foldr (op . f) u

Folds take an argument for each data constructor. Encodes it to a value of returned type. implementation deals with the recursive aspects.

### Monoids and Semigroups

Semigroups define a single `<>` (mappend) operation which lets you combine two values of the type.

Monoids have an additional method mempty which defines the _identity_

`mconcat` is available as a fold implementation. `foldr <> mempty`

There might be multiple Monoid implementations which might be possible. In that case its better to create a `newtype` which instead provides the implementation of the monoid. E.g. `Data.Semigroup.Sum`

Monoid Laws

```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

> [Monoids in Haskell](https://gist.github.com/cscalfani/b0a263cf1d33d5d75ca746d81dac95c5)
> [Monoids tour](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour)
> [Monoids and finger trees](https://apfelmus.nfshost.com/articles/monoid-fingertree.html)
> [Finger tree example](https://github.com/abbi-gaurav/haskell-projects/blob/a764a89cc265af71fe286829c9ff022aedd571e1/basics/src/monoids/RandomAccess.hs)

### Functors

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```
Defines a single fmap method which lets you transform the value inside a computation context. Defined for a type constructor of kind `* -> *`
map is fmap on lists.

Functor for a function is function composition.
```haskell
instance Functor ((->) r) where
    fmap = (.)
```

`fmap :: (a -> b) -> (f a -> f b)` If we pass a single function to `fmap` we get a function which accepts a functor and returns a different functor. Called _lifting_ a function.

#### Functor laws

* Mapping `id` gives the same functor back `fmap id = id`
* Mapping a function composition over a functor is the same as mapping 1st function over a functor and then the second . `fmap (f . g) = fmap f . fmap g`


Fmap doesn't let us map a function inside a functor with another functor.

### Applicative

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

Applicative takes a functor over a function and another functor and gives a functor with function applied over the value.

`pure f <*> x <*> y <*> ... a` lets us wrap functions in a functor and apply it on values which are in functor contexts.

`pure f <*> x` is the same as `fmap f x` . which is equivalent to `f <$> x` which is also provided by the Applicative module.


Examples:

```haskell
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
```

`liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c`


### Monads

`(>>=) :: (Monad m) => m a -> (a -> m b) -> m b `

```haskell
class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg
```

Examples:

```haskell
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f  = f x
    fail _ = Nothing

instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []

```
`return 1 :: Maybe Int`  Just 1
`return 1 >>= \x -> Just (x + 1)`  Just 2
`return 1 >>= \x -> Just (x + 1) >> Nothing`  Nothing

Using a do block:

```haskell
blah = do
    x <- Just 1
    Just (x + 1)
```

Monad laws

```haskell
return x >>= f  -- same as f x
m >>= return -- same as m
(m >>= f) >>= g -- same as m >>= (\x -> f x >>= g)
```

### IO

`(>>) :: IO a -> IO b -> IO b` also known as `and then`

Bind operator
`(>>=) :: IO a -> (a -> IO b) -> IO b`

