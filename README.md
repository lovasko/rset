# Data.Set.Range
[![Build Status](https://travis-ci.org/lovasko/rset.svg?branch=master)](https://travis-ci.org/lovasko/rset)

## API
The module offers a wide spectrum of operations on a set of ranges, mostly
aiming at mimicking the existing API of the `Data.Set` module. All types
handled by the data structure have to be instances of the `Eq`, `Ord` and
`Enum` typeclasses.

### Types
The module consists of two main types: `Range` which is a alias for a tuple,
denoting the low and high _inclusive_ boundaries of the range, and the
`RangeSet` type, which is an alias for a list of `Range` instances. The
`RangeSet` type should be treated in a opaque fashion due to portability
reasons, as it may become a `data` or `newtype` defined type in the future
releases of the module. Format definitions of the types mentioned above are:

```haskell
type Range a    = (a,a)
type RangeSet a = [Range a]
```

### Functions
All provided functions can be divided into the following 5 categories: list
conversions, combinations of multiple range sets, membership testing,
modifications of range set contents and general utility functions.

#### `empty :: RangeSet a`
Create an empty range set.

#### `null :: RangeSet a -> Bool`
Test whether the range set is empty.

#### `size :: (Num n, Enum a) => RangeSet a -> n`
Get the overall number of points.

#### `fromAscList :: (Ord a, Enum a) => [a] -> RangeSet a`
Create a range set from a list of ascending points. 

#### `fromDescList :: (Ord a, Enum a) => [a] -> RangeSet a`
Create a range set from a list of descending points.

#### `fromList :: (Ord a, Enum a) => [a] -> RangeSet a`
Create a range set from any list of points.

#### `toList :: Enum a => RangeSet a -> [a]`
Convert a range set into list of points.

#### `insertPoint :: (Ord a, Enum a) => a -> RangeSet a -> RangeSet a`
Insert a point into the range set.

#### `insertRange :: (Ord a, Enum a) => (a,a) -> RangeSet a -> RangeSet a`
Insert a range into the range set.

#### `removePoint :: (Ord a, Enum a) => a -> RangeSet a -> RangeSet a`
Remove a point from the range set.

#### `removeRange :: (Ord a, Enum a) => (a,a) -> RangeSet a -> RangeSet a`
Remove a range from the range set.

#### `queryPoint :: Ord a => a -> RangeSet a -> Bool`
Test whether a point is part of the range set.

#### `queryRange :: Ord a => (a,a) -> RangeSet a -> Bool`
Test whether a range is part of the range set.

#### `difference :: (Ord a, Enum a) => RangeSet a -> RangeSet a -> RangeSet a`
Select points contained only in the first range set.

#### `intersect :: (Ord a, Enum a) => RangeSet a -> RangeSet a -> RangeSet a`
Select points contained in both of the range sets.

#### `union :: (Ord a, Enum a) => RangeSet a -> RangeSet a -> RangeSet a`
Select points contained in either of the range sets.

## Complexity

| Function       | Time   | Space  |
|:---------------|:-------|:-------|
| `empty`        | O(*1*) | O(*1*) |
| `null`         | O(*1*) | O(*1*) |
| `size`         | O(*n*) | O(*l*) |
| `fromAscList`  | O(*n*) | O(*n*) |
| `fromDescList` | O(*n*) | O(*n*) |
| `fromList`     | O(*n*) | O(*k*) |
| `toList`       | O(*k*) | O(*n*) |
| `insertPoint`  | O(*k*) | O(*1*) |
| `insertRange`  | O(*k*) | O(*1*) |
| `removePoint`  | O(*k*) | O(*1*) |
| `removeRange`  | O(*k*) | O(*1*) |
| `queryPoint`   | O(*k*) | O(*1*) |
| `queryRange`   | O(*k*) | O(*1*) |
| `difference`   | O(*k*) | O(*k*) |
| `intersect`    | O(*k*) | O(*k*) |
| `union`        | O(*k*) | O(*k*) |

* *n* is the number of all points in the set
* *k* is the number of distinct ranges in the set
* *l* is the length of the largest range in the set

Given the example range set `[(1,4),(10,11)]` the variables above would be:
* `n` is 6
* `k` is 2
* `l` is 4

## Testing
The library is tested with the `QuickCheck` property testing library. All tests
are using the set functions from the standard library module `Data.List` to
verify the correctnes of `Data.Set.Range` functions. Moreover, the set of tests
is executed after every push to the GitHub repository by the Travis continuous
integration service.

The most recent test coverage reported by HPC is as follows:
```
100% expressions used (468/468)
 75% boolean coverage (12/16)
      75% guards (12/16), 4 always True
     100% 'if' conditions (0/0)
     100% qualifiers (0/0)
100% alternatives used (61/61)
100% local declarations used (6/6)
100% top-level declarations used (18/18)
```

## License
The `rset` package is licensed under the terms of the [2-clause BSD
license](LICENSE). In case you need any other license, feel free to contact the
author.

## Author
Daniel Lovasko <daniel.lovasko@gmail.com>
