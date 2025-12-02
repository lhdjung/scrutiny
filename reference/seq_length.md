# Set sequence length

`seq_length()` seamlessly extends or shortens a linear sequence using
the sequence's own step size.

Alternatively, you can directly set the length of a linear sequence in
this way: `seq_length(x) <- value`.

## Usage

``` r
seq_length(x, value)

seq_length(x) <- value
```

## Arguments

- x:

  Numeric or coercible to numeric. `x` must be linear, i.e., each of its
  elements must differ from the next by the same amount.

- value:

  Numeric (whole number, length 1). The new length for `x`.

## Value

A vector of the same type as `x`, with length `value`.

- If `value > length(x)`, all original element of `x` are preserved. A
  number of new elements equal to the difference is appended at the end.

- If `value == length(x)`, nothing changes.

- If `value < length(x)`, a number of elements of `x` equal to the
  difference is removed from the end.

## Examples

``` r
x <- 3:7

# Increase the length of `x` from 5 to 10:
seq_length(x, 10)
#>  [1]  3  4  5  6  7  8  9 10 11 12

# Modify `x` directly (but get
# the same results otherwise):
seq_length(x) <- 10
x
#>  [1]  3  4  5  6  7  8  9 10 11 12

# Likewise, decrease the length:
x <- 3:7
seq_length(x, 2)
#> [1] 3 4

seq_length(x) <- 2
x
#> [1] 3 4

# The functions are sensitive to decimal levels.
# They also return a string vector if (and only if)
# `x` is a string vector:
x <- seq_endpoint(from = 0, to = 0.5)
x
#> [1] "0.0" "0.1" "0.2" "0.3" "0.4" "0.5"

seq_length(x, 10)
#>  [1] "0.0" "0.1" "0.2" "0.3" "0.4" "0.5" "0.6" "0.7" "0.8" "0.9"

seq_length(x) <- 10
x
#>  [1] "0.0" "0.1" "0.2" "0.3" "0.4" "0.5" "0.6" "0.7" "0.8" "0.9"

# Same with decreasing the length:
seq_length(x, 2)
#> [1] "0.0" "0.1"

seq_length(x) <- 2
x
#> [1] "0.0" "0.1"
```
