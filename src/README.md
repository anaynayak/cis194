# Week 1 Intro

[CreditCard.hs](CreditCard.hs) using `unfoldr` , `swap`, `foldr`

# Week 2 ADT

[LogAnalysis.hs](LogAnalysis.hs) using `liftM3`, `Ord` instance. Need to check with a State monad.

# Week 3 Recursion

[Golf.hs](Golf.hs) using `zipWith`, list comprehensions.

# Week 4 Higher order programming

[Wholemeal.hs](Wholemeal.hs) using `iterate` and `where` . `where` [differs](https://wiki.haskell.org/Let_vs._Where) from `let` .

# Week 5 Type classes

[Calc.hs](Calc.hs) using `liftA2` instead of `pure (*) <*> (f1 m) <*> (f2 m)` . `flip`

# Week 6 Lazy evaluation

[Fibonacci.hs](Fibonacci.hs) `zipWith` and `tail` for a lazy fib sequence.
[Matrix.hs](Matrix.hs)

# Week 7 Folds, Monoids

[JoinList.hs](JoinList.hs) `Foldable` and `Monoid` instances for JoinList
[Scrabble.hs](Scrabble.hs) using `zip`, `fromList` to build a Map, score `Monoid` and `mconcat`.

# Week 8 IO

[Party.hs](Party.hs) `uncurry` and `bind` . `foldMap` instead of `mconcat` and `map`
