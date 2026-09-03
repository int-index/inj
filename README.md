# inj

A class for injective (one-to-one) functions.

An injection is a function that never maps distinct elements of the domain to
the same element of the codomain. For example, `\x -> x + 1` is an injection,
but `\x -> min x 0` is not.

```haskell
class Inj p a where
  inj :: p -> a
```

The instances compose, so `inj` can construct nested structures from singleton
elements, wrapping and converting as needed:

```haskell
ghci> inj 'a' :: Maybe [Char]
Just "a"

ghci> inj True :: Maybe [Bool]
Just [True]

ghci> inj (5 :: Int) :: Maybe Double
Just 5.0

ghci> inj [1, 2, 3 :: Int] :: [Double]
[1.0,2.0,3.0]

ghci> inj (True, 2 :: Int) :: (Maybe Bool, [Double])
(Just True,[2.0])
```

By convention, the instances of `Inj` never match on `p` and always match on
`a`. This guarantees that users will not encounter overlapping instances.

Instances for `base` types are provided by this package. Before version 2.0
they lived in a separate `inj-base` package; see the [changelog](CHANGELOG.md)
for migration notes.
