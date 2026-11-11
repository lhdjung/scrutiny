# AGENTS.md — scrutiny

Guidance for AI agents working on the scrutiny R package.

## Purpose

scrutiny implements statistical error-detection techniques for published
summary statistics — primarily consistency tests GRIM, GRIMMER, and
DEBIT. It also provides infrastructure (function factories) for
implementing new consistency tests. The package is tidyverse-oriented:
tidy data, tibbles throughout (never
[`data.frame()`](https://rdrr.io/r/base/data.frame.html)s), and
dplyr/purrr idioms.

## Style

Avoid overly verbose docs and comments. Don’t write walls of text. Use
readable variable names with English words, but still concise ones.

## Git

Make one commit each for every medium or major change. In commit
messages, don’t use “Co-Authored-By” lines. Start every paragraph after
the first line with “\*”. Before each commit, run `air format` on root.

## Architecture: the three-tier mapper hierarchy

Every consistency test exists at three levels, each created by a
function factory:

    *_scalar()          Single-case logic                   (not exported)
    *()                 Vectorized, e.g., grim()            map_value_sets()
    *_map()             Row-wise test on a data frame       function_map()
    *_map_seq()         Dispersed testing around values     function_map_seq()
    *_map_total_n()     Testing with unknown group sizes    function_map_total_n()

The factories live in:

| Factory | File |
|----|----|
| [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md) | `R/function-map.R` |
| [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md) | `R/function-map-seq.R` |
| [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md) | `R/function-map-total-n.R` |
| Shared helpers | `R/function-factory-helpers.R` |

Each factory uses
[`rlang::new_function()`](https://rlang.r-lib.org/reference/new_function.html) +
[`rlang::expr()`](https://rlang.r-lib.org/reference/expr.html) to
construct the body of the manufactured function at package-load time.
`!!` unquotes a single value into the body; `!!!` unquote-splices a
vector. This is metaprogramming, not string eval — treat the body
expressions with care.

The `.reported` argument to each factory is the character vector of key
column names (e.g., `c("x", "n")` for GRIM, `c("x", "sd", "n")` for
GRIMMER/DEBIT). It drives several automatic behaviours:
`insert_key_args()` adds `x = NULL`, `n = NULL`, … to the manufactured
function’s formals so users can rename columns;
[`absorb_key_args()`](https://lhdjung.github.io/scrutiny/reference/absorb_key_args.md)
applies those renames at call time.

[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
also has `.reported_variadic` for tests whose number of key columns is
decided by the caller’s data (#42). It names one argument of the
`*_scalar()` function that takes a whole row’s values as a vector; the
manufactured function gets an argument by that name which selects
columns with
[`tidyselect::eval_select()`](https://tidyselect.r-lib.org/reference/eval_select.html)
at call time. The selected columns are returned as themselves, so the
output shape is unchanged. Such a mapper is basic-tier only:
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
and
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
derive their own arguments from `.reported`, so `check_fn_wrappable()`
refuses it, as it refuses result columns named like those the tier adds.
[`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md)
is the one mapper built with it.

All three basic mappers —
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md),
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
— are one-liners calling
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
as the seq and total-n mappers have always been. There is no handwritten
mapper left in the package. Everything the three need is expressed
declaratively through these
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
arguments:

| Argument | Purpose |
|----|----|
| `.args_by_row` | args with one value per row (`digits_x`, `digits_sd`); become output columns |
| `.args_fixed` | values always passed to the `*_scalar()` function, with no argument exposed for them ([`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)’s `show_reason = TRUE`); passing one is a “disabled argument” error |
| `.cols_helper` | args that may also be `data` columns (`items_averaged`, `data_type`); returned as a column right after the key columns if given, as an argument or a column, and not returned at all if left at the default. A helper with no default must be given one way or the other |
| `.col_names` | names for the values a `*_scalar()` function returns under `show_rec` / `show_reason` |
| `.cols_derived` | columns computed from the same input but not returned by the `*_scalar()` function (`exp_incon` ← [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)); each function is vectorized and called once per mapper call |

Every other argument of the `*_scalar()` function automatically becomes
a real formal of the manufactured function with the same default — not a
dots argument. This is load-bearing: see the `digits_*` section below.
The seq and total-n factories follow suit: `formals_settings()`
(`R/function-factory-helpers.R`) copies `.fn`’s *settings* — its formals
after the dots, such as `rounding` — with `.fn`’s defaults. Before \#95
these went through the dots, hidden from
[`formals()`](https://rdrr.io/r/base/formals.html) and the help page. No
mapper’s dots take anything now; they only force the arguments after
them to be named in full.

A setting whose default is a set of strings, such as
`sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")`, is an
“enum” (`is_enum_default()`).
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
reassigns one left at its default to the first value before anything
else reads it, so the length check, the `*_scalar()` function, and the
`"scrutiny"` attribute all see a single string. One given as the whole
set, as a wrapper forwarding its own default does, is resolved the same
way by `resolve_enums()`, in
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
`map_value_sets()`, and
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
which reads `sd_meaning` for `n_min` before `fn()` runs. The
`*_scalar()` functions check enums against constants in `R/aaa.R`
(`SD_MEANINGS`, `DEBIT_FORMULAS`) rather than their own formals, which
is cheaper per row; a test in test-utils.R requires every default to
equal its constant.

`sd_meaning` is one argument on purpose: each value is one premise about
what a value reported as an SD really is (`sd_*` values: an SD of that
kind; `"se"`: a misreported SE). It replaced `sd_type` before 1.0.0
rather than gaining a second, combinable argument, because few
combinations of statistic and denominator occur in practice.
`sd_denominator(n, sd_meaning)` maps each premise to the number the sum
of squared deviations is divided by to give the reported value squared;
every place that turns the reported value into a sum of squares or a
binary spread goes through it, so a new premise of the same form is one
row there, plus one in `sd_denominator_ratio()`, which the
`sd_binary_*()` functions use to stay finite for huge `n`; a test
requires the two to agree. `"var"` would not be: its bounds are not
squared. Functions that return values under one premise, such as
[`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
and `sd_binary_*()`, reject `"sd_sample_or_pop"` via
`check_sd_meaning_single()`.

Arguments deliberately **not** carried over in the migration, because
they were sugar over one dplyr verb and blocked the unification: `extra`
(→
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
on the output), `testables_only` (→ `dplyr::filter(exp_incon > 0)`), and
`merge_items = FALSE`, which produced a packed `n` column that nothing
downstream can read. Don’t reintroduce them. Nor
`grimmer_map(show_reason)`, which only dropped a column; it is
`.args_fixed` now.
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
and
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
keep `show_rec`, whose default is `FALSE`: the reconstructed numbers are
intermediate results that few readers cite.
[`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
needs them and recomputes them under the recorded settings if they are
missing.

[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
and
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
no longer multiply `items` into `n`. The `n` column is the sample size
as given, and
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
places tiles at `n * items_averaged`.

`items_averaged` replaced `items` before 1.0.0 (#94) and, like
`digits_x`, has no default: the old default of 1 let multi-item scales
pass as single items, and “items” was mistaken for the number of scale
points. A missing one reaches the `*_scalar()` function as missing,
which calls `error_items_missing()` (`R/utils.R`) after the `digits_*`
checks. In mappers,
[`manage_helper_col()`](https://lhdjung.github.io/scrutiny/reference/manage_helper_col.md)
is then called without `default`, and an `items_averaged` column of
`data` satisfies the requirement. Only
[`grim_granularity()`](https://lhdjung.github.io/scrutiny/reference/grim_granularity.md)
keeps a default; it is no test.

`data_type` (`DATA_TYPES` in `R/aaa.R`: `"integer"`, `"continuous"`) is
a second helper of GRIM and GRIMMER, and DEBIT’s only one, with no
default, for the same reason: the tests only apply to whole numbers (for
DEBIT, 0 and 1). A missing one calls `error_data_type_missing()`. The
`*_scalar()` functions pass anything but `"integer"` to
`test_data_type()` (`R/utils.R`), which returns `verdict_continuous()`’s
result for continuous data and applies the NA rule to a missing value.
The `*_explain()` functions do the same through `explain_data_type()`,
and
[`grim_total()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
returns 0 for continuous data, so `exp_incon` follows; it and
[`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
require `data_type` too.
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
and
[`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
leave continuous rows out via `drop_continuous()`. The functions that
list or draw the values whole numbers can have (`*_values()`,
`*_closest()`, `*_plot_line()`) require `data_type` too, so that every
call states the assumption, but `"continuous"` is an error there
(`check_data_type_whole()`);
[`grim_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_grid.md)
follows
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md). GRIM’s
`x_meaning` (`"mean"` or `"percentage"`, `X_MEANINGS`) replaced the
logical `percent` before 1.0.0; it is an enum like `sd_meaning`. The
exports without dots keep `percent = deprecated()` to call
`stop_percent()`, and `check_factory_dots()` calls it for the mappers.

------------------------------------------------------------------------

## The `digits_*` argument pattern

Since v0.6, `x` and `sd` are stored as **numeric** (not character
strings). The number of decimal places therefore cannot be inferred from
the value itself and must be supplied explicitly.

- [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  requires `digits_x` (no default).
- [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  requires both `digits_x` and `digits_sd`.
- [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
  requires both `digits_x` and `digits_sd`.

A mapper also takes them from `digits_*` columns of `data` if the
arguments are left out: a column states them as explicitly as an
argument.
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
reads such columns like any `.args_by_row` argument; the seq factory
reads them back from its initial `fn()` call’s output, one per row, and
disperses each case on its own decimal level; the total-n factory
carries them along with the other columns of `data`, so a row’s value
applies to both groups.

If `digits_x` is missing, the scalar function calls
`error_digits_missing(x)` (`R/utils.R`) which gives a context-aware
error message with an example fix. Do not replace this with a generic R
missing-argument error.

`check_newly_numeric(x, digits_x)` (`R/utils.R`) validates that
`digits_x` is consistent with `x`.

### How `digits_*` flows through the factory stack

[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
computes `digits_args_names` as the intersection of
`paste0("digits_", .reported[.reported != "n"])` with
`names(formals(.fn))`. This means `digits_x` is auto-added to
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
[`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md),
and
[`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md)
(all three now require the corresponding `digits_*` args).

Inside the manufactured function body the digits args are collected into
`digits_vals` via `Filter(Negate(is.null), mget(...))` and forwarded
through both the initial
`do.call(fn, c(list(data), digits_vals, list(...)))` call and the
per-variable `map_seq_proto` calls via
`fn_args <- c(digits_vals, list(...))`.

------------------------------------------------------------------------

## Key files by subsystem

### Core tests

| File | Content |
|----|----|
| `R/grim.R` | `grim_scalar()` — single-case GRIM logic using [`roundwork::sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.html) |
| `R/grimmer.R` | `grimmer_scalar()` — Analytic-GRIMMER, whose test 1 also requires at least `sum_squares_min()`, the smallest sum of squares that whole numbers with the candidate sum can have, and for `n < 5` one that `sum_squares_attainable()` admits. For `n >= 5`, minimum and parity suffice, so the verdict is exact. Scale bounds (#60) were cut before 1.0.0 in favor of strait (#88) |
| `R/debit.R` | `debit_scalar()` / [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md), plus the DEBIT input-range checks |
| `R/grimmer-sum-squares.R` | `sd_square_term()`, `sum_squares_range_shifted()`, `sum_squares_min()`, `sum_squares_attainable()` (which sums of squares fewer than five whole numbers can have; see `special-scripts/grimmer-attainable-sums-of-squares.qmd`), and `sum_squares_attainable_all()`, all of them in a range — GRIMMER’s exact sum-of-squares arithmetic. The rules for three and four numbers live in `sum_squares_attainable_all()` only, vectorized over windows; for three, `loeschian_between()` generates the numbers `x^2 + xy + y^2` rather than testing each. The rounding bounds behind all three tests come from [`roundwork::bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.html) and [`roundwork::sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.html) (gotcha 6) |
| `R/grim-values.R` | [`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md), [`grim_closest()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md) — the achievable means behind a reported one; both thin wrappers over [`roundwork::sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.html). [`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md) returns one tibble (`case`, `x`, `sum`), like [`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md), stacked by `bind_cases()` from the per-set tibbles: an inconsistent value set has no rows, an undecidable one a row of `NA`. More than `VALUES_MAX` rows for one value set is an error from `check_values_size()` (`R/utils.R`), raised before they are allocated |
| `R/debit-values.R` | [`debit_values()`](https://lhdjung.github.io/scrutiny/reference/debit_values.md) — every number of ones whose mean and SD would be reported as the given ones; the k range from [`roundwork::sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.html), filtered by `binary_spread_range()`, as in `debit_scalar()`. `formula = "mean_n"` and `sd_meaning = "sd_sample_or_pop"` are errors there: no finite list, and two premises. [`debit_closest()`](https://lhdjung.github.io/scrutiny/reference/debit_values.md) picks the SD closest to `sd` from these, from the k whose mean rounds to `x`, or from [`grim_closest()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)’s k, in that order |
| `R/grimmer-values.R` | [`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md) — every (mean, SD) pair behind a reported one, with the whole-number sum and sum of squares; takes the verdict from `grimmer_scalar()`, then enumerates through `grimmer_candidate()` and `sum_squares_attainable()`. `"sd_sample_or_pop"` is an error there: one premise per call. For `n < 5` the pairs can’t be counted without finding them, so they are counted as found, against `VALUES_MAX`. [`grimmer_closest()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md) finds the pair whose SD is closest to `sd` without listing any: per remainder modulo `n`, the sum of squares nearest the target (`closest_square_parity()`, vectorized, for `n >= 5`; `closest_square_few()` below) |
| `R/grim-explain.R`, `R/grimmer-explain.R`, `R/debit-explain.R` | `*_explain()` — one case in plain words. Built from the numbers that decide the test: `grim_scalar(show_rec = TRUE)`, `grimmer_candidate()`, `binary_spread_range()`; GRIMMER and DEBIT abort on disagreement with the verdict |
| `R/explain-map.R` | [`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md), a generic like [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md), with methods for the three basic mappers’ classes, which also covers the seq and total-n tiers. Not `explain()`: that name is taken by dplyr, DALEX, and lime. Adds an `explanation` column of [`format()`](https://rdrr.io/r/base/format.html)ed explanations. Re-tests each row with the settings from `scrutiny_args()` and errors if the verdict differs from the row’s `consistency` |
| `R/explain-print.R` | [`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md) — [`cat()`](https://rdrr.io/r/base/cat.html)s the `explanation` column, with rows picked as in [`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html), always calling [`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md) on these rows, so that a user’s own `explanation` column is refused rather than printed. Other classes reach `explain_map.default()`, which errors, so packages can add methods; the check is not [`is_map_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md), which also admits [`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md) etc. [`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md) itself only checks for a tibble |
| `R/map-value-sets.R` | `map_value_sets()`, `recycle_length_common()` — the shared body of [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md), [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md), [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md), [`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md), [`grim_closest()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md), [`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md), and [`add_up_to()`](https://lhdjung.github.io/scrutiny/reference/add_up_to.md): applies a `*_scalar()` function once per value set, taking the caller’s arguments |

### Mappers

| File | Content |
|----|----|
| `R/grim-map.R` | [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md); one-liner calling [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md) |
| `R/grimmer-map.R` | [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md); one-liner calling [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md) |
| `R/debit-map.R` | [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md); one-liner calling [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md) |
| `R/grim-map-seq.R` | [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md); one-liner calling [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md) |
| `R/grimmer-map-seq.R` | [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md) |
| `R/debit-map-seq.R` | [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md) |
| `R/grim-map-total-n.R` | [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md) |
| `R/grimmer-map-total-n.R` | [`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md) |
| `R/debit-map-total-n.R` | [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md) |

### Dispersion / sequences

| File | Content |
|----|----|
| `R/seq-disperse.R` | [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md), [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md) |
| `R/disperse.R` | [`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md), [`disperse2()`](https://lhdjung.github.io/scrutiny/reference/disperse.md), [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md) — group-size variation |
| `R/seq-predicates.R` | [`is_seq_linear()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md), [`is_seq_ascending()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md), [`is_seq_dispersed()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md) |

### Decimal handling

| File | Content |
|----|----|
| `R/decimal-places.R` | [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md) (vectorized), `decimal_places_scalar()` (length 1, internal) |
| `R/restore-zeros.R` | [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md) — re-pad lost trailing zeros |

[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
counts from the string representation. Numeric values lose trailing
zeros, so always pass strings when exact decimal counts matter (e.g.,
`"3.70"` gives 2; `3.70` gives 1).

### Audit / summaries

| File | Content |
|----|----|
| `R/audit.R` | [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) S3 generic, [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md), [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md). The generic is `audit(data, ..., .by = NULL)`, so every method takes `(data, ...)`. `.by` never reaches a method: `audit_by()` splits with `[`, which keeps class and attribute, and calls [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) per group. `group_by()` keeps the attribute but drops the class, and `ungroup()` drops both; making the class survive would take methods for several dplyr verbs per class. `hint_grouped()` names the cause in `audit.default()` and `explain_map.default()` instead |
| `R/audit-cols-minimal.R` | [`audit_cols_minimal()`](https://lhdjung.github.io/scrutiny/reference/audit_cols_minimal.md) — shared base counts |
| `R/method-grim-map.R` etc. | S3 [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) methods for each test |
| `R/method-audit-seq.R` | [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md) implementation |

### Visualization

| File | Content |
|----|----|
| `R/grim-plot-grid.R` | [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md) — ggplot2 tile plot for GRIM results; its raster is `grim_grid_inconsistent()` |
| `R/grim-grid.R` | [`grim_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_grid.md) — GRIM’s verdict on every (`frac`, `n`) cell for one `digits` (1 to 3) and rounding method. `GRIM_GRIDS` (`R/sysdata.rda`, built by `data-raw/data-gen.R`) caches only the inconsistent cells, for 1 or 2 decimal places and every method without a `threshold`; `grim_grid_inconsistent()` reads it or calls `generate_grim_grid()`, and [`grim_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_grid.md) marks the full grid from it by position |
| `R/grim-plot-line.R` | [`grim_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_line.md) — comb of the means GRIM admits around one reported value; also `plot_comb()`, the helper both comb plots share |
| `R/grimmer-plot-line.R`, `R/debit-plot-line.R` | [`grimmer_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grimmer_plot_line.md), [`debit_plot_line()`](https://lhdjung.github.io/scrutiny/reference/debit_plot_line.md) — the same comb over SDs |
| `R/debit-plot.R` | [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md) |

### Utilities

| File | Content |
|----|----|
| `R/aaa.R` | `WHOLE_NUMBER_TOLERANCE`, `SD_MEANINGS`, `DEBIT_FORMULAS`, `VALUES_MAX`, and every other package-level constant |
| `R/utils.R` | `error_digits_missing()`, `check_newly_numeric()`, `check_digits_numeric()`, `write_code_col_key_result()`, `recycle_digits()`, `na_as_integer()`, misc helpers |
| `R/mapper-function-helpers.R` | [`check_mapper_input_colnames()`](https://lhdjung.github.io/scrutiny/reference/check_mapper_input_colnames.md), [`manage_helper_col()`](https://lhdjung.github.io/scrutiny/reference/manage_helper_col.md), `split_result_cols()`, `rethrow_row_error()` |
| `R/function-factory-helpers.R` | `insert_key_args()`, [`absorb_key_args()`](https://lhdjung.github.io/scrutiny/reference/absorb_key_args.md), `check_factory_dots()`, `check_factory_arg_names()`, `write_result_cols()`, `check_args_disabled()` |
| `R/data-frame-predicates.R` | [`is_map_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md), [`is_map_seq_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md), [`is_map_total_n_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md) |

------------------------------------------------------------------------

## S3 class system

Mapper output carries multiple classes stacked as a character vector:

    c("scrutiny_grim_map", "tbl_df", "tbl", "data.frame")

Classes are for dispatch only. Settings of the call that functions
downstream need live in one list attribute, `"scrutiny"`. Its `settings`
element is a tibble built by `new_settings()`
(`R/function-factory-helpers.R`): `setting`, `value` (a list-column of
the values themselves), `default`, and `added_by`.
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
writes the basic mapper’s rows — every argument that applied to the
whole call, defaults resolved — and the seq and total-n factories append
their own, such as `dispersion`. `default` compares each value to the
defaults that `eval_defaults()` evaluated when the factory made the
mapper; `added_by` is `name_mapper()`’s guess from `.name_test`
(`"grim_map"`), since a manufactured function doesn’t know the name it
was assigned to.

Package code reads typed values with the internal `scrutiny_args()`,
which returns the basic mapper’s rows as a named list — `rounding`
selects
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)’s
raster, `x_meaning` makes it apply the test’s conversion (divide `x` by
100, raise the decimal count by 2). The exported
[`scrutiny_meta()`](https://lhdjung.github.io/scrutiny/reference/scrutiny_meta.md)
is for people: it returns the tibble with `value` written as code in
single quotes, because pillar quotes and escapes a whole character
column as soon as one value has a double quote in it. Don’t read it in
package code. `.args_fixed` values are not recorded: they are settings
of the mapper, not of the call. Attributes and classes survive exactly
the same dplyr verbs, so the move changed nothing there; it replaced
regex parsing of class strings and made accidental dispatch on a flag
impossible. Storing all the arguments rather than a chosen few is what
made `.name_class_flags` unnecessary. Don’t add flag classes again.

- `scrutiny_{test}_map` — drives
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  dispatch, and the
  [`pillar::tbl_sum()`](https://pillar.r-lib.org/reference/tbl_sum.html)
  methods in `R/scrutiny-meta.R`, which add a header line naming the
  settings whose `default` is `FALSE`. pillar pads header names to the
  widest, so the lines are returned unnamed with the names pasted in. A
  new basic mapper needs such a method to get the line
- `scrutiny_map_seq` / `scrutiny_map_total_n` — the generic tier
  classes; these are what
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  and
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  dispatch on
- `scrutiny_{test}_map_seq` / `scrutiny_{test}_map_total_n` — the
  test-specific tier classes. They exist so that users can write their
  own S3 methods for one particular mapper’s output. The total-n half
  was documented in `?audit-special` but never set until the 1.0.0 audit
  fixes;
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
  now adds it the way
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
  always has.

`*_map_seq()` output takes the `"scrutiny"` attribute of its initial
`fn()` call and adds its own settings rows and elements to it (see
gotcha 16), setting it explicitly after assembly rather than relying on
it to survive
[`purrr::list_rbind()`](https://purrr.tidyverse.org/reference/list_c.html).
Total-n output does the same with the attribute of its `"forth"` half,
plus a `reported` element: the statistics reported for both groups,
which
[`reverse_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_total_n.md)
reads because a custom basic mapper may put `n` anywhere among its key
columns.

------------------------------------------------------------------------

## `grim_plot_grid()` and mixed decimal places

[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md),
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md),
and every mapper built on top of them (`*_map_seq()`, `*_map_total_n()`)
attach the `digits_x` (and, where applicable, `digits_sd`) they were
given as an output column.
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
reads decimal places from that column instead of guessing via
`decimal_places(data$x)`, which is unreliable for numeric `x`: trailing
zeros are lost, so `5.00` reads back as 0 decimal places, not 2 (see
gotcha 6 below). Guessing from `x` is only a fallback for data with no
`digits_x` column, e.g. a hand-built tibble.

[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
errors by default if the decimal counts still differ across rows after
that (i.e. the data itself mixes decimal places), because the background
raster is only meaningful for one specific decimal count, and a single
call should have a stable return type (one `ggplot`, not sometimes a
`ggplot` and sometimes a list). Set `split_by_digits = TRUE` to opt into
the alternative:
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
then splits the data by distinct non-zero `digits_x` values, produces
one plot per group, and returns a named list
(`list(digits_1 = ..., digits_2 = ...)`) invisibly. Each plot is also
printed as a side-effect. A
[`cli::cli_alert_success()`](https://cli.r-lib.org/reference/cli_alert.html)
message reports the count and decimal values when more than one plot is
made.

------------------------------------------------------------------------

## `function_map_seq()` internals — things to know

`function_map_seq_proto()` (the inner helper factory) manufactures a
per-variable test function. Its inner function has `...` that are
forwarded to `fn(...)`. These `...` are **not** the outer function’s
dots — they come from the call site of the proto function.

The outer `fn_out` body captures the arguments given (e.g., `digits_x`,
`rounding`) into `fn_args <- c(digits_vals, setting_vals, list(...))`
and forwards them via
`do.call(map_seq_proto, c(list(data = data, var = variable), fn_args))`.
If you need to add new args that must reach `fn()`, include them in
`fn_args` or make them explicit args like the `digits_*` treatment
above.

`digits_*` columns are added to the output for every non-`n` reported
variable (e.g., `digits_x` for GRIM, `digits_x` + `digits_sd` for
GRIMMER/DEBIT) and relocated to appear immediately before `consistency`.

Limits that depend on a setting are functions of the resolved settings,
as a named list.
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
takes them as `.var_bounds`
(`n = function(args) c(sd_min_n(args$sd_meaning), NA)` in the GRIMMER
and DEBIT seq mappers),
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
as `.n_min`; there the manufactured function’s `n_min` defaults to
`NULL`, which the body resolves, and the resolved value is what the
recorded setting counts as the default. A constant works in both places
as before.

------------------------------------------------------------------------

## Coding conventions

- **Tibbles**: always use
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html),
  or, in performance-sensitive cases where `nrow` is known,
  [`tibble::new_tibble()`](https://tibble.tidyverse.org/reference/new_tibble.html);
  never [`data.frame()`](https://rdrr.io/r/base/data.frame.html).
- **Error messages**:
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  with named vector elements (`"!"`, `"x"`, `"i"`). Never
  [`stop()`](https://rdrr.io/r/base/stop.html). For errors about user
  input, use `abort_in_export()`, which names the exported function the
  user called, wherever on the stack the check runs;
  `name_last_export()` gives that name for the message text. Never count
  frames. Every error that is not a bug in scrutiny goes through it; an
  internal error, one that only a bug can cause, is
  `cli::cli_abort(..., .internal = TRUE)`. These come from
  `R/import-standalone-last-export.R`, imported from unsum with
  `usethis::use_standalone()`: change them upstream, never by hand.
- **Warnings/info**:
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html) /
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html).
- **Scalar helpers**: functions that operate on a single case are named
  `*_scalar()` and not exported. They have a thin vectorized wrapper,
  e.g.,
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).
- **File naming**: matches the exported function with `-` not `_` as
  separator (e.g., `grim-map-seq.R` exports
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)).
- **Constants**: package-level constants live in `R/aaa.R` and are named
  in `SCREAMING_SNAKE_CASE`, so that a name elsewhere is recognizable as
  one on sight. They are evaluated at install time, which for the
  tolerance is deliberate: a verdict must not depend on the machine.
  `R/aaa.R` is first in `Collate` because roxygen2 sorts unconstrained
  files alphabetically.
- **`@include` roxygen tag**: used to guarantee correct `NAMESPACE`
  order; do not remove existing `@include` directives unless you intend
  to change collate order.
- **`check_factory_dots()`**: called near the top of every manufactured
  function body. It checks that `...` args are valid formals of `fn`.
  Args that are explicit named params (like `digits_x`) are not in `...`
  and therefore not checked here — that is expected.
- **No function written inside
  [`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html)
  that reads a variable.** dplyr inlines it, and the variable is then
  looked up among the columns of `data` first: a column named `sep`
  replaced the argument of
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md).
  Define the function before the `mutate()` call.
- **`integer64` is rejected, not converted.** It is a double by type, so
  [`is.numeric()`](https://rdrr.io/r/base/numeric.html) is `TRUE`, but
  only bit64 can read its values. `check_not_integer64()` (`R/utils.R`)
  runs once per call: by name in
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
  for the tested columns and in `map_value_sets()`, and in
  `round_whole()` for every other export that takes a whole number.
- **Numbers become strings through `number_to_string()`**
  (`R/decimal-places.R`), never a bare
  [`as.character()`](https://rdrr.io/r/base/character.html) whose result
  is parsed: that writes the decimal mark of `options(OutDec = )`.
- **Never `rlang::enexprs(...)` where a value is needed.** Inside a
  manufactured function body, capturing the dots as expressions yields
  the caller’s *expression* — a symbol if the caller passed a variable —
  unless the promises happen to have been forced already. Use
  `list(...)` for values. The seq factory’s rounding-class fixup got
  this wrong once and only worked by accident.

------------------------------------------------------------------------

## Testing

Tests live in `tests/testthat/`. The framework is `testthat >= 3.0.0`.

- `helper.R` — general helpers: `unclass_scr()`, `censor()`,
  `integer_places()`, `sweep_seed()` (a new seed per run outside of
  CRAN; `SCRUTINY_SEED` replays one)
- The full DEBIT oracle in `test-debit.R` runs only with
  `SCRUTINY_FULL_ORACLE=true`, as in the weekly
  `.github/workflows/full-oracle.yaml`
- `test-mapper-oracle.R` re-tests every row of every mapper tier with
  the vectorized test; a new setting or mapper belongs there too. See
  `special-scripts/oracle-tests-assessment.md` for all the sweeps
- `helper-expectations.R` — custom expectations: `expect_na()`
- Snapshot tests use
  [`testthat::expect_snapshot()`](https://testthat.r-lib.org/reference/expect_snapshot.html);
  regenerate with
  [`testthat::snapshot_review()`](https://testthat.r-lib.org/reference/snapshot_accept.html)
  when output changes intentionally.

### Test style

Follow commit c3abc01 (`test-utils.R`) as the model:

- A pipe chain starts with the object, never with a call on it: write
  `x |> f1() |> f2()`, not `f1(x) |> f2()`. Only a compound first
  argument (`list(a, b)`, `c(...)`, `tibble::tibble(...)`) starts the
  chain as a call.
- Expectations go at the end of the chain:
  `x |> f(y = 1) |> expect_error("msg")`, not
  `expect_error(f(x, y = 1), "msg")`.
- Several `|>` on one line are fine, and preferred: don’t break a line
  after a pipe. A chain that is broken after one pipe is broken after
  every pipe — never half and half.
- In consecutive parallel lines, align the pipes with spaces to show the
  parallels. `air.toml` skips `test_that()` blocks, so the formatter
  leaves them alone there; at the top level, end each aligned (or
  over-long parallel) line with `# fmt: skip`.
- Leave `expect_snapshot()` code alone; the snapshot files record it.

When modifying a factory (`function-map-seq.R` etc.), run tests for
*all* three mappers that use it (GRIM, GRIMMER, DEBIT), not just the one
under immediate focus.

------------------------------------------------------------------------

## Gotchas and non-obvious behaviours

1.  **`digits_x` has no default** in
    [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
    /
    [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md).
    Calling them without it, and without a `digits_x` column in `data`,
    hits `error_digits_missing()`, not a generic R error. This is
    intentional — the error message guides users.

2.  **`seq_disperse_df_internal()`** is a lightweight version of
    [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
    used inside `function_map_seq_proto()`. Both wrap
    [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md),
    so a fix there reaches the mappers too. `out_min` and `out_max`
    apply to `from` itself as well: a reported value outside of them is
    dropped even under `include_reported = TRUE`, so `seq_disperse(0)`
    has no `"0"` under the default `out_min = "auto"`.

3.  **[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
    et al. are one-liners** that call
    [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md).
    There is no hand-written function body. All logic lives in the
    factory. When debugging manufactured functions, check the factory
    body in `R/function-map-seq.R`, not the one-liner in
    `R/grim-map-seq.R`.

4.  **`*_map_seq()` output records the settings of the initial `fn()`
    call**, such as `rounding` with its default resolved, by copying
    that call’s `"scrutiny"` attribute onto the assembled output. The
    re-tests get the same arguments, so the record holds for every row.

5.  **`insert_key_args()` inserts after `data` and the `digits_*` args**
    — `insert_after = 1L + length(<by-row args>)` in all three
    factories. The `digits_*` args have no defaults and must be given in
    every call, so they sit next to `data`, in the same position in
    every mapper. The dots come right after the key arguments, so every
    argument after them must be named in full: `items = 2` partially
    matched `items_averaged`, and `n` in a total-n mapper both `n_min`
    and `n_max`. The total-n mappers have an `n` key argument for the
    column of totals. Final signature order:
    `data, digits_x, x, n, ..., items_averaged, data_type, …` for
    [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
    `data, digits_x, x, n, ..., items_averaged, data_type, x_meaning, …, var, dispersion, …`
    for
    [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
    and
    `data, digits_x, x1, x2, n, ..., items_averaged, data_type, x_meaning, …, dispersion, …`
    for
    [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md):
    the basic mapper’s settings come right after the helpers in all
    three tiers.
    [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
    reorders `formals_promoted` to put the `.args_by_row` args first; do
    that *after* `args_required` is derived, since that line pairs
    `formals_promoted` with `args_promoted` by position.

    Each factory works out its own `digits_args_names` as the
    intersection of `paste0("digits_", <its reported vars>)` with
    `names(formals(.fn))`, so a test that takes no decimal places at all
    gets no such arguments. The total-n factory took them through the
    dots until the 1.0.0 audit fixes: that worked, and the
    missing-argument error was still the bespoke one, but the argument
    was invisible to [`formals()`](https://rdrr.io/r/base/formals.html),
    to tab-completion, and to the rendered help page.

    Helper arguments got the same treatment for the same reason. The seq
    and total-n factories read `.fn`’s `"scrutiny_args_helper"`
    attribute and copy those formals with `.fn`’s defaults, so
    `items_averaged` has none there either. The body collects the ones
    given into `helper_vals` as its very first step, before any local
    could shadow one, and leaves out missing ones so that they reach
    `fn()` as missing. The seq factory passes them to the initial `fn()`
    call only, not to `fn_args`: the re-tests read them from the column
    that call returned.

    The settings (`formals_settings()`) are collected into
    `setting_vals` the same way, right after `helper_vals`, but go to
    both the initial call and `fn_args`. One left out is not forwarded,
    so `fn()` applies its own default, which is the same.

6.  **Rounding lives in roundwork.** `reround()`, `unround()`, the
    `round_*()` functions, `bound_numerators()`, `sum_range()`, and
    `resolve_ties_rounding()` are roundwork’s, called as
    `roundwork::*()`. Fix rounding there, not here: until 1.0.0,
    scrutiny had its own copy, and the two drifted apart by some 90
    commits. `bound_numerators()`, `sum_range()`, and
    `resolve_ties_rounding()` are low-level primitives that check little
    of their input, so scrutiny checks it before calling them.

    roundwork does check `rounding`, `threshold`, and `symmetric`, but
    its errors name the roundwork function that scrutiny called, such as
    `sum_range()`. Every scrutiny export that takes them therefore calls
    `check_rounding_args()` (`R/utils.R`) once, before any per-row work:
    via `map_value_sets()`, the
    [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
    body, the `*_explain()` functions, and
    [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md).
    It asks roundwork for the bounds of a stand-in value and re-raises
    any error through `rethrow_in_export()` as one from the user’s
    export. A new export with a `rounding` argument needs this too,
    unless it goes through one of those.

7.  **The `"ties_*"` rounding methods are shorthand.** Each names a
    complete tie-breaking procedure, so it stands in for a `rounding`
    plus a `symmetric`, and giving `symmetric = TRUE` with one is an
    error. Anything in scrutiny that branches on `rounding` itself has
    to resolve them with
    [`roundwork::resolve_ties_rounding()`](https://lhdjung.github.io/roundwork/reference/resolve_ties_rounding.html)
    —
    [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
    does, to pick the background raster. The oracle grid in
    `test-grim-oracle.R` skips `symmetric` for these methods, which
    costs no coverage: the name alone decides them.

8.  **No rounding method has undefined bounds.**
    [`roundwork::bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.html)
    returns `NULL` only for a missing or infinite `x`. `"anti_trunc"` at
    zero used to be another case, and much of the package’s
    `NA`-propagation machinery was written with it as the example; since
    `anti_trunc()` became `ceiling(abs(x))` (matching Excel’s
    `ROUNDUP()`), zero has the single-point range `0 <= x <= 0`. Use a
    missing value or a non-positive `n` when you need an undecidable
    case.

9.  **[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
    counts from string representation.** Numeric `3.70` returns `1`, not
    `2`. This means `digits_x` from `decimal_places(x)` on a numeric
    column can be wrong if trailing zeros have been lost. In `*_map()`
    and `*_map_seq()` output the `digits_x` column is **not** computed
    via
    [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
    on the values — it is set directly from the `digits_x` argument
    provided by the caller (via `recycle_digits()` in the basic mappers)
    and replicated for every output row. This is reliable even when
    trailing zeros have been lost from the numeric `x` column.

    Non-finite values return `NA`, not `0`: `NaN` is a missing value
    everywhere in the package, and an infinity has no decimal places to
    count.
    [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
    decides this from the *input* —
    [`is.finite()`](https://rdrr.io/r/base/is.finite.html) for a numeric
    vector, a case-insensitive match against the `inf`, `infinity`, and
    `nan` tokens for a character one, plus the exact string `"NA"`,
    which
    [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
    reads as missing too — because
    [`as.numeric()`](https://rdrr.io/r/base/numeric.html) would also
    swallow strings like `"5.30%"`, whose two decimal places are the
    documented answer. `decimal_places_scalar()` has to agree with it; a
    generated corpus in `test-decimal-places.R` checks that they do.

    Two rules say which strings are missing, by what a function reads
    (`R/aaa.R`, at `NON_FINITE_TOKENS`): functions that read *numbers*
    from strings follow
    [`as.numeric()`](https://rdrr.io/r/base/numeric.html), so `"nan"`
    and `"inf"` in any letter case are non-finite; functions that read
    *any text*, such as
    [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md),
    take only `"NaN"` as R writes it, via `is_value_missing()`, because
    `"Nan"` is a name. Don’t unify them.

    An all-`NA` vector of any type is `NA` of the argument’s type:
    `na_as_integer()` (`R/utils.R`), called from `round_whole()` and
    `as_integer_if_lossless()`, turns a logical or character `NA` column
    of `n`, `items_averaged`, or `digits_x` into `NA_integer_`, and
    `na_as_character()` does the same for `data_type` (`ARGS_STRING`). A
    `NULL` or an empty vector is left alone: `NULL` means “none” to
    `disperse_total(n_max)`. A `digits_*` column of strings is refused
    up front by `check_digits_numeric()`, since every row would fail and
    the per-row error would blame none.

10. **[`absorb_key_args()`](https://lhdjung.github.io/scrutiny/reference/absorb_key_args.md)**
    modifies `data` before any other processing. If you add
    pre-processing that reads column names, it must happen after this
    call. It reads the key arguments’ *values* in the calling mapper’s
    frame (`capture_key_args()`, via
    [`rlang::enquo()`](https://rlang.r-lib.org/reference/enquo.html)),
    never the unevaluated call: through
    [`lapply()`](https://rdrr.io/r/base/lapply.html) the call is
    `FUN(X[[i]], ...)`, and from a wrapper it names a variable. A bare
    name is a column name if `data` has that column, else the string a
    variable of that name holds. A key argument pointing elsewhere while
    `data` already has a column of the argument’s own name is an error,
    not silently ignored.

11. **`write_code_col_key_result()`** is called at the very end of
    manufactured function bodies. It may rename the `consistency` column
    and coerces list-columns to logical. Any column
    additions/relocations must happen before this call. Only
    [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
    passes `rename = TRUE` (the default): it is the tier that creates
    the column. The seq and total-n factories build on a basic mapper’s
    output, where the column already carries its final name, and pass
    `rename = FALSE` so that only the
    [`unlist()`](https://rdrr.io/r/base/unlist.html) half of the
    generated code applies.

    `.name_key_result` is threaded through all three factories.
    Everything that reads the key result column off `.fn`’s output does
    so by that name — the seq factory’s filter of consistent cases and
    its `digits_*` relocation, `function_map_total_n_proto()`’s
    `mutate_both_consistent()`,
    [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md),
    and
    [`reverse_map_seq()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_seq.md).
    The last two learn the name from the `name_key_result` element of
    the `"scrutiny"` attribute that
    [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
    puts on its output, falling back to `"consistency"` when the
    attribute is gone. A mapper and its sequence or total-n mapper must
    be created with the *same* `.name_key_result`;
    `check_key_result_col()` says so if they were not.

12. **Every mapper applies its test to the first row on its own, before
    mapping over all of them.** Arguments of a `*_scalar()` function
    without a default (i.e., `digits_x` and `digits_sd`) must reach it
    as missing, so that `error_digits_missing()` fires rather than a
    generic R error;
    [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
    therefore skips them when collecting the arguments to pass down. The
    pre-application is what keeps the resulting error out of
    [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)’s
    indexed-error context, and it runs unconditionally: anything wrong
    with the arguments themselves fails on the first row just as it
    would on any other. The rounding arguments are checked even before
    that (gotcha 6). It used to run only for missing required arguments.

    Do **not** muffle warnings or messages in that call. It looks right,
    since the row is about to be tested again, but a once-per-session
    warning is then lost for good:
    [`lifecycle::deprecate_warn()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
    records that it has warned before the muffle discards the warning,
    so the deprecated `grim_map(tolerance)` (removed before 1.0.0) went
    through in complete silence. Nothing on the scalar test path warns
    per row today.

    An error in any later row still comes from inside
    [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html),
    which wraps it in an “In index: 14” context naming its own anonymous
    function. `rethrow_row_error()` (`R/mapper-function-helpers.R`)
    re-signals the test’s own error instead, with an “In row 14 of
    `data`” bullet and a `scrutiny_row` field. The total-n proto
    function reads that field and rewrites the bullet to the row of the
    user’s `data`, since a row of its rebuilt data is a hypothetical
    scenario.

    An error in the first row goes through `signal_in_row()` too, but
    only if some other row passes (`passes_any_row()`, which tries rows
    2 to 50 and the last): an error about the arguments, such as a
    missing `digits_x`, fails on every row and must not blame row 1.

13. **`special-scripts/`** contains development aids and GRIMMER
    debugging material. It is not part of the built package.
    `special-scripts/dormant/` holds the rivets and metadata files that
    used to sit in `R/` as ~1,500 lines of fully commented-out code, ~9%
    of the package’s source, walked through by every reader, grep, and
    coverage tool. Put unfinished work there, not in `R/`.

14. **`grim_map(show_rec = TRUE)` returns the same columns for every
    rounding method**: `rec_sum`, `sum_lower`, `sum_upper`,
    `rec_x_upper`, `rec_x_lower`. `sum_lower`/`sum_upper` come straight
    from the `sum_range()` call that decided `consistency`, so display
    and verdict cannot come apart. Up to 1.0.0 the columns were the two
    granules re-rounded — a second derivation of the verdict, which
    under `rounding = "anti_trunc"` could contradict it. Don’t
    reintroduce a display that is computed separately from the verdict.

15. **Zero-row input.** `write_result_cols()` cannot read the output’s
    shape off a result that doesn’t exist, so a mapper called on a 0-row
    `data` tests a row of missing values instead and returns the columns
    of that result with no rows. If `.fn` fails on missing values, only
    the key result column is returned. Total-n output with no pair of
    group sizes gets its columns the same way. A `*_map_seq()` call with
    no inconsistent case returns 0 rows too, but with the usual columns,
    classes, and attribute, so
    [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
    and
    [`reverse_map_seq()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_seq.md)
    accept it. Undecidable cases are never dispersed, not even under
    `include_consistent = TRUE`; the warning counts them.

16. **Attributes tie the mapper tiers together.**
    [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
    records its helper-column argument names (e.g., `items_averaged`) as
    the `"scrutiny_args_helper"` attribute of the mapper it
    manufactures. A helper that was given is a column of the mapper’s
    output, so
    [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
    keeps the helper args out of the arguments forwarded to the
    re-tests, which read them from the column, and
    [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
    carries such columns into the data it rebuilds for `fn()`. Until
    1.0.0,
    [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
    multiplied `items` into `n` and both factories had to undo that;
    GRIMMER uses `n - 1` and `items^2`, so testing the product gave
    wrong verdicts. The seq mapper in turn records the verdict of every
    input row from its initial `fn()` call as the `results_reported`
    element of the `"scrutiny"` attribute of its *output*.
    [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
    reports these for the cases present, indexed by `case`, instead of
    re-testing reconstructed data: until the 1.0.0 design review it
    recovered the mapper’s *name* from a class string, looked the
    function up in the caller’s environment, and replayed the call’s
    arguments from a `fn_args` element, so a dropped argument could flip
    the verdict and the mapper had to be findable from wherever
    [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
    ran. Verbs like `summarise()` and `pivot_longer()` drop the
    attribute (`[`, [`filter()`](https://rdrr.io/r/stats/filter.html),
    and `bind_rows()` keep it, the last only for pieces of one output:
    `R/bind-rows.R` registers vctrs methods that reject output of
    different calls, whose settings or verdicts differ, or rows of a
    plain data frame; base
    [`rbind()`](https://rdrr.io/r/base/cbind.html) is not covered.
    Column binding passes: `vec_cbind()` asks the same methods with
    zero-column inputs);
    [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
    then errors, like
    [`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md).
    The same attribute holds `name_key_result` (see gotcha 11). No order
    of `dispersion` steps needs recording: each row’s `diff_var` says
    how far it was moved. `var`, the dispersed variables, from which
    [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
    shapes its output when there are no rows, is a row of `settings`.
    Seq and total-n output also record `values_reported`, the key
    columns of their input: their `case` numbers restart at 1 per call,
    so without it two calls with the same settings would pass the
    `bind_rows()` guard. They also record the rows of each group,
    `rows_dispersed` per case and `var` and `rows_tested` per case and
    `dir` (`count_rows_by()`, `R/audit.R`): a group with fewer rows lost
    some to [`filter()`](https://rdrr.io/r/stats/filter.html) or the
    like, so
    [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
    and
    [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
    say `NA` for it.
    [`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md)
    records `cols_variadic`, the columns its summands selected.

17. **A test that cannot be decided says `NA`, and all three agree on
    what those cases are.** `is_decidable_n_items()` (`R/utils.R`) is
    the single condition — stated twice, in `&&` form for the single
    values the `*_scalar()` functions pass it once per row and in `&`
    form for the columns
    [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
    passes it once per call. The two have to be kept identical; a test
    in test-utils.R runs both over the same grid and requires them to
    agree. Do not collapse them into one: `&&` neither allocates nor
    evaluates what it does not need, which is most of the per-row cost,
    and looping the scalar form over a column would cost far more than
    it saves. The condition itself: `n` and `items_averaged` must be
    positive whole numbers, and `n` must be at least `2` for GRIMMER and
    DEBIT, which reconstruct a *sample* SD and so divide by `n - 1`.
    GRIM’s `min_n` is `1` — the mean of a single value is that value.

    A missing `x` or `sd` (or `digits_x`/`digits_sd`) is `NA` only if it
    could decide the case either way. GRIMMER with `sd` missing is
    `FALSE` if `x` fails GRIM; with `x` missing, it checks the sums
    `0:(n - 1)`, which stand for all sums by their remainder modulo `n`,
    and is `FALSE` if none admits the SD. DEBIT with `sd` missing is
    `FALSE` if no number of ones rounds to `x` (`"exact"` only); with
    `x` missing, if no number of ones (or, under `"mean_n"`, no mean)
    has the SD. `combine_sd_sample_or_pop()` keeps `"sd_sample_or_pop"`
    at `NA` when the sample SD is open and the population SD fails. The
    `*_explain()` functions have paragraphs for these cases. A value
    whose `digits_*` is missing has at least the decimal places it shows
    (`digits_least()`, `R/utils.R`), and more only narrow its rounding
    range: if the value set is `FALSE` with these, it is `FALSE`. If it
    is `TRUE` with so many that only the value shown passes
    (`digits_exact_mean()`, `digits_exact_sd()`), it is `TRUE`; not
    under DEBIT’s `"mean_n"`. Else the value counts as missing. A
    negative SD is `FALSE` even with `n` missing. A missing `n` or
    `items_averaged` gives `TRUE` if a value set passes with 1 in its
    place (GRIM: copies of the values keep the mean; GRIMMER and DEBIT:
    data that are all the same, with an SD of 0), and with `n` missing,
    GRIMMER and DEBIT give `FALSE` for an SD below `sd_below_floor()`’s
    floor or, for DEBIT, above the largest a binary sample can have
    (`debit_n_missing()`). GRIM with `x` missing is `TRUE` if every mean
    passes (`grim_all_means_pass()`).
    [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
    stays `NA` for a missing `n`: its number depends on it. The rule is
    frozen there:
    [`vignette("technical-notes")`](https://lhdjung.github.io/scrutiny/articles/technical-notes.md),
    section *Missing values*, lists the cases that stay `NA` although
    every value would give the same result, and
    [`?grim`](https://lhdjung.github.io/scrutiny/reference/grim.md),
    [`?grimmer`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
    and
    [`?debit`](https://lhdjung.github.io/scrutiny/reference/debit.md)
    point to it. A change to these cases needs that list changed too.

    The tolerance accepts a value like `100 * 0.29` (28.999999999999996)
    as whole, so it is also tested as 29: `round_whole()` (`R/utils.R`)
    rounds the `ARGS_WHOLE` arguments (`R/aaa.R`) at the export
    boundaries, before any arithmetic or comparison with `min_n` –
    [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)’s
    `cols_tested`, `map_value_sets()`, the `*_explain()` and
    `*_plot_line()` functions,
    [`grim_total()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
    and
    [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
    the `sd_binary_*()` functions,
    [`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
    and
    [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
    and the total-n body’s `data$n`. A new export that reads these
    arguments itself, rather than through one of those, needs the same
    call. The `*_scalar()` functions never see the noise. Fractional
    values are left alone and rejected as before.

    That `2` is `sd_min_n(sd_meaning)` since \#95:
    `sd_meaning = "sd_pop"` divides by `n` (`sd_denominator()`,
    `R/utils.R`), so `n = 1` is testable there. `"sd_sample_or_pop"`
    re-runs the `*_scalar()` function under both conventions: `TRUE` if
    the sample SD passes, otherwise the population SD’s verdict. That is
    `|` everywhere except at `n = 1`, where no sample SD exists, so it
    cannot leave the verdict `NA`. The re-run comes after the
    `missing(digits_*)` checks on purpose: a missing argument passed on
    from a closure is no longer
    [`missing()`](https://rdrr.io/r/base/missing.html) in the callee, so
    `error_digits_missing()` would not fire. `sd_meaning` and `formula`
    are validated with `arg_match_in_export(sd_meaning, SD_MEANINGS)`
    etc., whose fast path is ~8 times cheaper per row than a bare
    [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html).

    Before the 1.0.0 audit fixes each test made its own arrangements and
    none of them covered fractional input: `grim(x = 5.19, n = 20.5)`
    was `FALSE`, `grimmer(items = 1.5)` was `TRUE`, `grimmer(n = 1)` was
    `FALSE` through a `NaN` that `na.rm = TRUE` swallowed, and
    `debit(n = 1)` was `FALSE` through an `Inf` that compared as “above
    the upper bound”.
    [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
    uses the same condition, so the `exp_incon` column cannot report a
    number next to an `NA` verdict.

18. **[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
    returns its plot; it does not print it.** The one exception is the
    `split_by_digits = TRUE` branch, which returns a *list* that
    auto-printing cannot draw, so it prints each plot itself and returns
    the list invisibly. The function used to end on
    [`print()`](https://rdrr.io/r/base/print.html) unconditionally,
    which drew a canvas nobody asked for whenever the result was
    assigned, added to, or composed.

    It also reports what it leaves out rather than letting ggplot2 drop
    rows: value sets whose `consistency` is `NA` have no color to be
    drawn in, and `digits_x = 0` rows have a fractional portion of zero
    and no raster. Both are warnings naming the count; if *nothing* is
    left, that is an error.

19. **`data <- as_tibble_input(data)` is the first statement in every
    manufactured mapper body** — in all three factories, ahead of
    [`absorb_key_args()`](https://lhdjung.github.io/scrutiny/reference/absorb_key_args.md)
    and of anything else that reads column names. It admits any data
    frame and returns a tibble, keeping character row names as a
    `rowname` column (integer ones, as left by `data[rows, ]`, are
    dropped, and a `rowname` column next to row names is an error);
    anything else is an error. Keep it first. A non-data-frame has no
    column names to speak of, so a key-column check running ahead of it
    reports missing columns and blames the user’s data for the wrong
    object — worst of all for an undefined `data`, which R resolves to
    [`utils::data()`](https://rdrr.io/r/utils/data.html). Until the
    1.0.0 design review, a `data.frame` was rejected with a conversion
    hint, which hit tables from
    [`read.csv()`](https://rdrr.io/r/utils/read.table.html) on the first
    call.

20. **[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
    reaches every tier.** Sequence and total-n output carries the basic
    mapper’s class too, after its own tier classes, so the methods
    `audit.scrutiny_map_seq()` and `audit.scrutiny_map_total_n()`
    (`R/audit.R`) are what keep
    [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
    from summarizing dispersed rows as reported cases. Study-level GRIM
    statistics,
    [`grim_binomial()`](https://lhdjung.github.io/scrutiny/reference/grim_binomial.md)
    and
    [`grim_plot_rates()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_rates.md),
    reject those tiers through `check_grim_map_basic()` for the same
    reason. Any new function that reads mapper output as a set of
    *reported* value sets needs the same guard.

21. **[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
    [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
    and
    [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md)
    write out their own signatures.** They used to be
    `Vectorize(grim_scalar)` and the like, which copies *every* formal
    of the wrapped function — including `show_rec` and `show_reason`,
    which only the mapper tier has any use for, and which are the only
    way to make a `*_scalar()` function return something other than a
    logical value. [`mapply()`](https://rdrr.io/r/base/mapply.html) then
    simplified that list return into a matrix. The signatures are now
    hand-written, minus those arguments, and share one body:
    `map_value_sets()` (`R/map-value-sets.R`).

    A wrapper’s signature must mirror its `*_scalar()` function’s,
    because `map_value_sets()` forwards only the arguments the caller
    actually supplied and lets `.fn`’s own defaults apply to the rest.
    That one rule is what keeps a missing `digits_x` reaching
    `grim_scalar()` as missing, so that `error_digits_missing()` fires
    (gotcha 12 again, one tier up).

    `along` names the arguments to vectorize over. The split is the
    mapper tier’s own: key columns, `.args_by_row`, and helper columns
    are vectorized; everything that
    [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
    passes as a constant for the whole call (`rounding`, `threshold`,
    `symmetric`, `x_meaning`, `formula`, `sd_meaning`) must be length 1.
    Recycling of the `along` arguments follows the tidyverse rules and
    errors on a mismatch, where
    [`mapply()`](https://rdrr.io/r/base/mapply.html) recycled anything
    into anything and merely warned.

    Which arguments were supplied is worked out with
    [`missing()`](https://rdrr.io/r/base/missing.html), evaluated in the
    wrapper’s frame. Do not swap in
    [`match.call()`](https://rdrr.io/r/base/match.call.html): it is
    cheaper, but a wrapper reached through
    [`lapply()`](https://rdrr.io/r/base/lapply.html) is called as
    `FUN(X[[i]], ...)`, and matching that call fails with “… used in a
    situation where it does not exist”.

22. **`check_newly_numeric()`’s
    [`round()`](https://rdrr.io/r/base/Round.html) test is one-sided,
    and must stay that way.** The check asks whether `x` fits in
    `digits` decimal places. `x == round(x, digits)` answers that some
    200 times faster than counting the decimal places in
    `as.character(x)`, which is what `decimal_places_scalar()` does with
    three regular expressions — once per key value per row, formerly the
    most expensive part of a mapper call (#92). The test only ever
    *passes* a value; whatever it leaves undecided falls through to the
    count, which is unchanged. Two cases depend on that fallback: a
    double that prints as a short decimal without being that decimal’s
    double (`0.1 + 0.2` prints as `"0.3"`), and a value whose integer
    part is longer than the 15 significant digits
    [`as.character()`](https://rdrr.io/r/base/character.html) renders.
    Never widen the test into a tolerance-based one: the tolerance that
    would forgive floating-point noise overlaps the differences the
    count treats as real, so the two would disagree above about 15
    significant digits.

    The whole-number test for `digits` is inlined against
    `WHOLE_NUMBER_TOLERANCE` rather than calling `is_whole_number()`.
    That constant exists because the old default argument,
    `.Machine$double.eps^0.5`, was a list lookup plus a square root on
    every call — more than the arithmetic it is a tolerance for.
    `is_decidable_n_items()` writes the same test out twice for the same
    reason (gotcha 18).

    `decimal_places_scalar()` branches on `is.character(x)` so that a
    numeric value skips the `"Inf"`/`"NaN"` match, which only a string
    can fail, and skips the exponent regexes unless a fixed-string
    search finds an `e` or `E` first. Both are guards against work, not
    changes to what is counted;
    [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
    must keep agreeing with it, and the generated corpus in
    test-decimal-places.R is what checks that.

    An infinite `x` or `sd` is undecidable, like a missing one. It needs
    no branch here — [`round()`](https://rdrr.io/r/base/Round.html)
    returns it unchanged, so it passes — and `grim_scalar()`,
    `grimmer_scalar()`, and `grim_sums_scalar()` each return `NA` for it
    rather than letting it reach `sum_range()`, whose infinitely wide
    range read as GRIM-consistent. DEBIT needs no such branch: its input
    checks already confine `x` and `sd` to `[0, 1]`.

23. **GRIMMER and DEBIT have no 2^53 limit of their own.** A double
    holds whole numbers exactly only below 2^53, and a value set on a
    rounding bound gets either verdict if a comparison goes past it, so
    no such arithmetic may degrade to floating point.
    `R/exact-arithmetic.R` has two levels: `compare_products()`,
    `product_difference()`, and `floor_product_ratio()` (Dekker’s
    two-product) for products of two factors below 2^53, cheap enough
    for the common path, and `big()` with the `big_*()` functions for
    whole numbers of any size (base-2^24 limbs), used only when a factor
    or result passes 2^53. `whole_*()` functions take either: a value is
    a classed `big()` number exactly when it is at least 2^53. Bounds
    are put in lowest terms (`lowest_terms()`) first.

    GRIMMER works on each candidate sum `s = n * q + r` shifted to `r`:
    subtracting `q` from every value keeps the deviations, so the sums
    of squares depend on the SD alone, not on the mean
    (`sum_squares_range_shifted()`).
    [`grimmer_explain()`](https://lhdjung.github.io/scrutiny/reference/grimmer_explain.md)
    and
    [`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
    show the unshifted sums of squares (`grimmer_candidate_absolute()`),
    which they can only do below 2^53; beyond, the explanation says so
    and
    [`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
    errors. The one refusal left is `n` from 2 to 4 with shifted sums of
    squares from 2^40 on (`check_grimmer_attainable_exact()`): an SD of
    about 3e5 to 1e6, which no study reports for so few values, and
    beyond which `loeschian_between()` takes seconds and gigabytes for
    `n = 3`. `n = 1` is decided exactly at any size. Apart from that, a
    million candidate sums (`check_enumeration_size()`) are the limit,
    with the mean known or not; when the sums cover every remainder
    modulo `n`, only those within reach of the SD’s term are tried.

    **Rule out unrealistic sizes rather than compute them.** Where
    exactness at sizes no study reaches keeps causing defects or slow
    paths, refuse the input with an error instead of adding more
    machinery. GRIM’s own limit on `x * n * 10^(digits_x + 1)`
    (`check_sum_range_exact()`, also used by
    [`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md))
    comes from
    [`roundwork::sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.html)
    and remains.
