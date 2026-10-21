# Open items from the audit of 2026-10-21

The audit found no wrong test verdict. Every defect it confirmed was fixed in
the six commits from 06b47de to 1d64c85. The items below were left open
because each needs a decision, not a patch. None affects a verdict.

## 1. `items` partial-matches `items_averaged` in the mappers

`grim_map(df, digits_x = 2, items = 1)` runs: R's partial matching binds
`items` to `items_averaged`, because `...` comes last in every mapper's
signature. The name `items` was removed in #94 for being misread as the
number of scale points, and nothing warns when it is used.

Fix: put `...` before `items_averaged` (and the other helper args) in the
formals that `function_map()`, `function_map_seq()`, and
`function_map_total_n()` build. Arguments after `...` must then be named in
calls, which they are anyway. `check_factory_dots()` would then reject
`items`. Changes the signatures, so it is a 1.0.0 decision.

## 2. `n = "total"` in a total-n mapper is ambiguous

`grim_map_total_n(df, ..., n = "total")` fails with "argument 4 matches
multiple formal arguments": `n` partially matches `n_min` and `n_max`. The
total-n mappers have key arguments `x1` and `x2` but none for `n`, so the
total column cannot be renamed at all.

Fix: either an `n` key argument like `x1`/`x2`, or the same `...` move as in
item 1 so that the base R message is replaced by scrutiny's "Column `n`
missing" error. The first is the useful one.

## 3. `constant` in the total-n mappers is not swapped for `dir = "back"`

`disperse_total()` documents `constant` as a pair of values attached to the
two group sizes, and its example reads them as group statistics (`c("5.24",
"3.80")`). In `*_map_total_n()`, `x1` and `x2` change places in the `"back"`
direction, but `constant` stays with the group-size position, so in `"back"`
rows `x2` is listed next to `constant1`. `constant` is also one pair per
call, so for multi-row `data` it cannot describe the groups anyway.

Decide what `constant` means in a mapper (a property of the group, of the
position, or neither) and document it, or drop it from the mappers and keep
it in `disperse_total()` only. Dropping is the smaller change.

## 4. `is_numeric_like()` and the string `"NA"`

`restore_zeros()` and `decimal_places()` read the string `"NA"` as a
missing value (`NON_FINITE_TOKENS` and the exact `"NA"` match), but
`is_numeric_like(c("1.5", "NA"))` is `FALSE`, since `as.numeric("NA")` is
`NA`. So `restore_zeros_df()` skips a column that `restore_zeros()` would
process, and `decimal_places_df()` warns that it is not numeric-like. The
same holds for `"NaN"` and `"Inf"` as strings, which `is_numeric_like()`
accepts only because `as.numeric()` does.

Decide whether `is_numeric_like()` should treat these tokens as missing, as
the two string readers do. If so, `is_numeric_like_col()` is the place, since
the exported predicate's documented contract ("every non-`NA` value can be
coerced to a non-`NA` number") is literally met today.

## 5. `decimal_places()` on very small doubles

`decimal_places(1e-300)` is 315, because `as.character(1e-300)` is
`"9.99999999999999e-301"`. R renders some doubles with 15 nines, so the count
is off for them even though the number has one significant digit. A scan of
every `k / 10^d` for `d` up to 4 found no such case, so reported statistics
are unaffected. At most, mention it in the docs next to the 15-digit
limitation.

## 6. `add_up_to_map()` renames before it selects

`add_up_to_map(df, summands = c(n_control, sum), total = "sum")` fails with
tidyselect's "Column `sum` doesn't exist" rather than the dedicated "has a
role already" error, because the key renaming runs before the variadic
selection. The order is a documented choice in `function_map()`; only the
message is off.

## 7. Stray file

`tests/testthat/testthat-problems.rds` is an untracked leftover of a failed
test run on 2026-10-20. The suite passes; delete the file.
