---
editor_options: 
  markdown: 
    wrap: 72
---

# Issues #69 and #83 — fixes

Work done on the `polishing` branch (v0.6.2). Started from the two issues
named in the title. Fixing them uncovered three further defects on the
same code path; a follow-up pass over the vignettes uncovered two more.
All of them were present at `HEAD` before this work. File paths are
relative to the package root.

Test suite after the changes: **773 passing, 0 failures.** All eleven
vignettes build. `R CMD check` is clean apart from three pre-existing
findings (see *Left undone*).

------------------------------------------------------------------------

## Summary

| # | Defect | Fix |
|---|--------|-----|
| 1 | [#83](https://github.com/lhdjung/scrutiny/issues/83): `dispersion` fails at 305 | Round dispersed values back to their decimal level (`R/seq-disperse.R:148`) |
| 2 | [#69](https://github.com/lhdjung/scrutiny/issues/69): factory-made functions can't reach scrutiny helpers | `function_map_total_n()` no longer encloses its output in the caller's environment (`R/function-map-total-n.R:588`) |
| 3 | `digits_*` error messages throw internal errors | Resolve the caller by scanning the stack, not by counting frames (`R/utils.R:314-374`) |
| 4 | Same bug in `name_caller_call()` | Reuse the new name resolution, with a fallback (`R/utils.R:1495`) |
| 5 | `audit_seq()` rejects its own output for custom tests | Only add `digits_*` columns the mapper accepts (`R/function-map-seq.R:450`) |
| 6 | Vignettes still call the pre-1.0.0 API | Rewrote six vignettes for `digits_x` / `digits_sd` (section 6) |
| 7 | `*_map_seq()` steps by the stored value, not `digits_*` | Take the step size from the `digits_*` argument (`R/function-map-seq.R:44`) |
| 8 | Scalar `digits_*` can't describe mixed-decimal data | `digits_x` / `digits_sd` accept one value per row (section 8) |

Defects 3-5 and 7-8 were found while verifying the ones before them.
Each was reproduced on a clean worktree at `HEAD` before being fixed,
and each new test was confirmed to fail when its fix is reverted.

------------------------------------------------------------------------

## 1. Issue #83 — dispersion fails at 305

### Symptom

``` r
grim_map_seq(pigs5, digits_x = 2, dispersion = 1:304)   # fine
grim_map_seq(pigs5, digits_x = 2, dispersion = 1:305)   # error
#> ! More decimal places than specified digits.
#> ✖ `digits_x` is 2.
#> ✖ `x` is 0.0899999999999999, so it has 16 decimal places.
```

The issue reported this as an error inside `restore_zeros()`, which is
where it surfaced on 0.6.x while `x` was still a string. Since `x`
became numeric, the same cause surfaces in `check_newly_numeric()`
instead. `restore_zeros()` itself is not at fault.

### Cause

`seq_disperse()` built its sequence in plain floating-point arithmetic:

``` r
out <- append(rev(from - disp_minus), c(disp_zero, from + disp_plus))
```

`pigs5` contains `x = 3.14`, and `3.14 - (305 * 0.01)` is
`0.0899999999999999`, not `0.09`. The dispersed value then has 16
decimal places, so `grim_map()` rejects it against `digits_x = 2`.

The sequence is *defined* to proceed on the decimal level of `by` (or,
when `by` is not given, of `from`), so any value that leaves that level
is an artifact.

### Fix

`R/seq-disperse.R` derives the intended decimal level once and rounds
back to it at each of the four points where a value is computed from
`from`, `by`, and `dispersion`:

``` r
digits_out <- max(digits, decimal_places_scalar(from))
```

`max()` covers a manually specified `by` with fewer decimal places than
`from` — e.g. `from = 3.14, by = 0.1` must round to 2, not 1.

Rounded at: the `out_min` comparison, the `out_max` comparison, the
`offset_from` shift, and the final sequence.

### Second symptom, same cause

Rounding *before* the range comparisons also fixes a quieter bug: a
value landing exactly on `out_min` or `out_max` was dropped.

``` r
seq_disperse(from = 0.6, dispersion = c(3, 5, 6),
             track_diff_var = TRUE, include_reported = FALSE)
```

The `-5` step lands on `0.6 - 5 * 0.1`, which is exactly `0.1` — the
value `out_min = "auto"` resolves to — and so belongs in the sequence.
In floating point it is `0.09999999999999998`, hence seemingly below the
minimum.

An existing expectation in `tests/testthat/test-seq-disperse.R:63`
encoded the buggy output. It was updated, with a comment explaining why
the `-5` step belongs there.

### Verified

-   `grim_map_seq()`, `grimmer_map_seq()`, `debit_map_seq()`, and
    `grim_map_total_n()` all handle long `dispersion` vectors, with
    output staying at 2 decimal places throughout.
-   `dispersion = 1:305` output is identical to `dispersion = 1:304`
    output on the rows they share.
-   `debit_map_seq()` still errors for dispersions that push `x` above
    1. That is correct: DEBIT means must be in [0, 1], and no `out_max`
    is set.

------------------------------------------------------------------------

## 2. Issue #69 — helpers unreachable from factory-made functions

### Status of the issue as written

The issue proposed exporting a list of internal helpers, or inlining
them. The comment on the issue is right that this is not needed in
general: a factory-made function is enclosed in an environment
descending from the `function_map*()` execution frame, whose parent is
scrutiny's namespace, so bare calls to internal helpers resolve.

That holds for two of the three factories. `function_map_total_n()` was
the exception.

### Cause

`R/function-map-total-n.R` passed an explicit environment to
`rlang::new_function()`:

``` r
env = rlang::caller_env()
```

That is the environment of whoever *called* `function_map_total_n()`.
Created inside scrutiny it happens to work; created in another package
it does not:

``` r
#> Error in absorb_key_args(data, reported_reduplicated) :
#>   could not find function "absorb_key_args"
```

`function_map()` relied on `new_function()`'s default (the
`function_map()` execution frame — correct, but implicit), and
`function_map_seq()` already used `rlang::env()`.

### Fix

All three factories now use `env = rlang::env()`, which creates a child
of the current execution environment and therefore inherits scrutiny's
namespace. Each carries a comment stating the constraint, so it is not
re-broken.

No exports were needed. This is the alternative the issue text allowed
for ("Some calls to functions that are currently exported may currently
lack a `scrutiny::` namespace spec").

Note that `function_map()` already qualifies five helpers as
`scrutiny::absorb_key_args()` etc. Those are all exported, so they keep
working; the qualification is redundant but harmless.

### Verified

`tests/testthat/test-function-factories-env.R` does two things:

1.  Asserts that each factory's output has scrutiny's namespace among
    the ancestors of its enclosing environment.
2.  Exercises all three factories from an
    `new.env(parent = baseenv())` standing in for another package's
    namespace, and compares results against the same call made from
    within scrutiny's scope.

Reverting to `caller_env()` makes assertion 1 fail.

------------------------------------------------------------------------

## 3. `digits_*` error messages threw internal errors

### Symptom

``` r
debit_map_seq(pigs3, dispersion = 1:2)
#> Error in as.character(rlang::caller_call(n + 1)[[1]]) :
#>   cannot coerce type 'closure' to vector of type 'character'

scrutiny::grim_map(pigs1)
#> Error in `lifecycle_message()`:
#> ! `what` must be a single string, not a character vector.
```

The user sees an internal error where the guidance message was supposed
to be — on the very path that exists to explain the 1.0.0 `digits_*`
API change.

### Cause

`caller_fn_name()` was:

``` r
caller_fn_name <- function(n = 1) {
  as.character(rlang::caller_call(n + 1)[[1]])
}
```

The head of a call is not always a symbol:

-   `scrutiny::grim_map(...)` → the head is the call
    `scrutiny::grim_map`, and `as.character()` returns
    `c("::", "scrutiny", "grim_map")` — length 3, which
    `lifecycle::deprecate_stop()` then rejects.
-   A factory-made function invokes `fun` as a *function object* via
    `do.call()`, so the head is a closure and `as.character()` throws.

Compounding this, the frame count `n` was hard-coded (`n <- 3`, with an
ad-hoc `+1` for `mapply`). How many frames separate a `*_scalar()`
function from the call the user typed depends on what sits in between:

| Call | Frames between |
|------|----------------|
| `grim_map(pigs1)` | purrr's `pmap_lgl` machinery |
| `grim(4.11, 40)` | `Vectorize()` → `do.call()` → `mapply()` |
| `debit(0.35, 0.18, 20)` | `debit_table()`, then the above |
| `debit_map_seq(pigs3)` | `do.call()` → unnamed factory-made frame |
| `grim_map_total_n(df)` | `map_total_n_proto()`, then purrr |

No fixed number is correct for all of these.

### Fix

`R/utils.R` (lines 314-374) replaces `caller_fn_name()` and `caller_fn_names_all()` with
two helpers:

-   `fn_name_from_call()` (`:314`) — the name a call invokes, as a
    single string. Handles a symbol, a `pkg::fn` call (returns the bare
    name), another callable head such as `obj$method`, and a function
    object (returns `""`).
-   `caller_test_fn()` (`:354`) — scans `sys.calls()` for the outermost
    frame whose name matches `^(grim|grimmer|debit)`, and returns its
    name and frame. `sys.calls()` runs outermost-first, so the first
    match is the one the user called.

`check_newly_numeric()` and `error_digits_missing()` now take both the
name and the error attribution from that result, instead of counting
frames.

### Result

All nine call paths produce the intended message. Sequence and total-n
mappers now name *themselves* rather than the basic mapper they call
internally:

``` r
debit_map_seq(pigs3, dispersion = 1:2)
#> ! Need to specify `digits_x` to state the number of decimal places in `x`.
#> ℹ For example, with 1.40 (two decimal places):
#>   `debit_map_seq(tibble::tibble(x = 1.4, sd = 0.62, n = 29,
#>    digits_x = 2, digits_sd = 2))`
```

``` r
grim_map_seq(tibble::tibble(x = "4.11", n = 40), digits_x = 2, dispersion = 1:2)
#> ! Using string `x` columns in `scrutiny::grim_map_seq()` was deprecated in
#>   scrutiny 1.0.0 and is now defunct.
```

### Coverage

These paths had **no tests at all**.
`tests/testthat/test-error-digits.R` now covers, for each of the nine
call shapes: which function the message names, the mapper-vs-basic
example form, the GRIMMER/DEBIT `sd` parts, the string-`x` deprecation,
and the "more decimal places than digits" branch.

------------------------------------------------------------------------

## 4. Same bug in `name_caller_call()`

Found by building the vignettes. `consistency-tests-simple.Rmd` — the
documented workflow for building your own mapper — died at
`audit_seq()`:

``` r
#> Error in paste0("`", name, "()`") :
#>   cannot coerce type 'closure' to vector of type 'character'
```

`name_caller_call()` (`R/utils.R:1495`) took `name[[1L]]` from the
caller's call and pasted it, with the same two failure modes as above.
It is used by seven call sites across `function-factory-helpers.R` and
`mapper-function-helpers.R`, so it affects error messages throughout the
factory system.

It now delegates to `fn_name_from_call()`. When the caller was invoked
as a function object there is no name to report, so `"the function"`
stands in for one — without backticks, since it is not code.

------------------------------------------------------------------------

## 5. `audit_seq()` rejected its own output for custom tests

Once the message in §4 became readable, it named a real defect:

``` r
#> ! Invalid argument `digits_y`.
#> ✖ It's not an argument of the function or `schlim_scalar()`.
```

### Cause

Two pieces of code disagreed about which `digits_*` columns exist.

`function_map_seq()` added one for **every** reported variable except
`n`:

``` r
.digits_col_names <- paste0("digits_", reported[reported != "n"])
```

`audit_seq()` (`R/audit.R:209`) forwards **every** `digits_*` column in
the output back to the mapper as an argument:

``` r
digits_cols <- grep("^digits_", colnames(data), value = TRUE)
```

For GRIM, GRIMMER, and DEBIT these line up. For a mapper built from
one's own test — which has no `digits_*` arguments at all — the column
was still added, forwarded back, and rejected.

### Fix

The factory already computes which `digits_*` arguments the mapper
actually has:

``` r
digits_args_names <- intersect(
  paste0("digits_", .reported[.reported != "n"]),
  names(formals(.fun))
)
```

`R/function-map-seq.R:450` now uses that vector for the output columns
too, so the two sides agree by construction. Unchanged for GRIM,
GRIMMER, and DEBIT — verified that their `digits_x` / `digits_sd`
columns are still present and still relocated before `consistency`.

### Note for the test

`audit_seq()` recovers the mapper by evaluating the name derived from
the output's class (`scrutiny_schlim_map_seq` → `schlim_map`). It does
this with `rlang::eval_bare()` from inside `audit_seq()`, so the lookup
runs through scrutiny's namespace and *not* through the caller's
environment. A user's own mapper must therefore live in the global
environment. The test assigns it there and removes it on exit.

This is worth knowing but was not changed here.

------------------------------------------------------------------------

## Files changed

| File | Change |
|------|--------|
| `R/seq-disperse.R` | Round dispersed values to their decimal level |
| `R/function-map.R` | Explicit `env = rlang::env()` |
| `R/function-map-seq.R` | Explicit env comment; `digits_*` columns from `digits_args_names` |
| `R/function-map-total-n.R` | `caller_env()` → `rlang::env()` |
| `R/utils.R` | `fn_name_from_call()`, `caller_test_fn()`; rewrote caller resolution in `check_newly_numeric()`, `error_digits_missing()`, `name_caller_call()` |
| `NEWS.md` | Four entries |
| `tests/testthat/test-seq-disperse.R` | Corrected one expectation; decimal-level tests |
| `tests/testthat/test-grim-map-seq.R` | `dispersion = 1:305` regression test |
| `tests/testthat/test-error-digits.R` | New — 22 assertions |
| `tests/testthat/test-function-factories-env.R` | New — factory environments, `digits_*` columns |
| `tests/testthat/test-digits-vectorized.R` | New --- per-row `digits_*`, sequence step size |
| `vignettes/*.Rmd` | Six vignettes rewritten for the `digits_*` API |
| `man/*.Rd` | Regenerated for the `digits_*` documentation |

------------------------------------------------------------------------

## 6. Vignettes

Six of the eleven vignettes still called the pre-1.0.0 API and could not
be built: `grim`, `grimmer`, `debit`, `wrangling`,
`consistency-tests-simple`, and `consistency-tests-in-depth`. All eleven
now render, and `R CMD check` reports
`re-building of vignette outputs ... OK`.

The mechanical part was replacing quoted values with numeric ones and
adding the right `digits_x` / `digits_sd` to every call. Beyond that:

-   The passages explaining *why* values had to be strings were rewritten
    to explain the `digits_*` arguments instead — including the section
    in `wrangling.Rmd` that the other vignettes link to.
    `restore_zeros()` is now framed as a tool for when you want the zeros
    back in the values themselves, not as the way to prepare data for a
    consistency test.
-   The copy-and-paste instructions no longer tell readers to add
    quotation marks with multiple cursors.
-   `split_by_parens(transform = TRUE)` returns strings, so
    `wrangling.Rmd` now converts them with `as.numeric()` and reads the
    digits off the still-intact trailing zeros via `decimal_places()`.
-   `options(pillar.sigfig = 4)` was added to the setup chunks. With
    numeric `x` columns, pillar's three-significant-digit default printed
    a reported `10.46` as `10.5`, which is a poor look in vignettes about
    counting decimal places.
-   One claim in `grimmer.Rmd` no longer matched its output: the
    `flying_pigs1` example now has one inconsistent value, not two. That
    follows from the earlier GRIMMER correctness fixes, not from the API
    change. The sentence was corrected.

Six chunks still show errors on purpose (`grim_plot(mtcars)`, DEBIT on
non-binary data, and so on). Those were checked one by one against the
surrounding prose; the rendered HTML contains no other errors.

------------------------------------------------------------------------

## 7. `*_map_seq()` ignored `digits_*` when choosing its step size

Found because it kept `debit.Rmd` from building.

`function_map_seq_proto()` passed each value to `seq_disperse()` without
a `by` argument, so the step size came from `decimal_places()` of the
stored number. A mean reported as 5.30 is stored as `5.3`, so it was
dispersed in steps of 0.1 while a neighbouring 4.71 was dispersed in
steps of 0.01 — in the same call, under the same `digits_x`, and with
both rows labelled `digits_x = 2` in the output.

In `debit_map_seq(pigs3, digits_sd = 2, dispersion = 1:7)` the oversized
steps pushed SDs up to 1.2, outside the range DEBIT accepts, so the call
failed outright.

The proto now derives the step size from the caller's `digits_*` value
for the variable being dispersed (`R/function-map-seq.R:44`). There is no
`digits_n`, so `n` keeps stepping by whole numbers.

------------------------------------------------------------------------

## 8. `digits_x` and `digits_sd` accept one value per row

The `grim_map_total_n()` case study in `vignette("grim")` reproduces
Bauer and Francis (2021), who report means of 4.71 and 5.3 for a total
sample size of 40. Those have different numbers of decimal places, which
a single `digits_x` cannot express. Testing 5.3 as "5.30" is a stricter
test, and it collapsed the section's findings from three hits to one,
losing the entire *Testing both ways* argument.

The `digits_*` arguments therefore now take more than one value:

-   `grim_map()`, `grimmer_map()`, `debit_map()`: length 1 (the whole
    column) or `nrow(data)` (one per row). The values ride along as a
    column of the data frame that `purrr::pmap()` iterates over, so every
    row reaches its `*_scalar()` function with its own.
-   `grim_map_total_n()` and friends: length 1 or 2, stating the decimal
    places of the two groups. `function_map_total_n_proto()` recycles the
    pair across the alternating group rows, and swaps it in the `"back"`
    direction, where the pairing is reversed.
-   `*_map_seq()`: still one value per column. The step size, the
    filtering of consistent cases, and the `digits_*` output columns would
    all have to follow the same per-row mapping. A vector now raises a
    clear error instead of failing deep inside `seq_disperse()`.

With `digits_x = c(2, 1)`, the case study returns `hits_forth = 1`
(17/23, what Bauer and Francis found) and `hits_back = 2` (19/21 and
16/24), matching every claim in the vignette prose.

A related bug was fixed along the way: the error message for a missing
`digits_*` argument suggested
`grim_map(tibble::tibble(x = 1.4, n = 29, digits_x = 2))`, which puts the
argument *inside* the data frame and so reproduces the very same error.
It now reads `grim_map(tibble::tibble(x = 1.4, n = 29), digits_x = 2)`.

------------------------------------------------------------------------

## Left undone

**Three pre-existing `R CMD check` findings** were left alone:

-   `'::' or ':::' import not declared from: 'poibin'` — from
    `R/binomial.R`, introduced with `grim_binomial()`.
-   Non-standard top-level files: `AGENTS.md`, `CLAUDE.md`,
    `Rplots.pdf`.
-   Leftover LaTeX logs in `vignettes/`:
    `grimmer-terminating-decimals.log` and
    `grimmer-terminating-decimals-academic.log`.

**`check_newly_numeric()` has an unused `caller_type` argument.** It is
still validated with `rlang::arg_match()` but never read. Untouched.
