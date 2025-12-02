# Absorb key arguments from the user's call

If `insert_key_args()` is called in the exit area of a function factory
(i.e., after the part that produces the factory-made function),
`absorb_key_args()` must be called in the main part. Unlike the former,
it transforms `data`, not `fun`, and should be reassigned to `data`.

## Usage

``` r
absorb_key_args(data, reported, key_cols_call)
```

## Arguments

- data:

  User-supplied data frame.

- reported:

  String. Names of the key arguments.

- key_cols_call:

  User-provided arguments named after one or more key columns.

## Value

Data frame `data`, possibly with one or more columns renamed. Remember
reassigning the value to `data`!

## Details

It renames key columns that have non-standard names, following
user-supplied directions via the arguments automatically inserted below
the function.

## Examples

``` r
# Not really a meaningful example -- need to use
# the function in very specific places
data <- grim_map(pigs1)
data <- absorb_key_args(data, c("x", "n"))
```
